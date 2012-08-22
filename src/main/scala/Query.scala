package css.query

import util.parsing.combinator.RegexParsers
import java.lang.RuntimeException

import xml.{ NodeSeq, Node }

trait CssImplicits {
  implicit def nodeSeqToCssSelect(nodes: NodeSeq) = new RichCssSelection(nodes)
}

class RichCssSelection(origin: NodeSeq) {
  def select(input: String)(implicit parser: CssParsers) = parser(input)(origin)

  def $(input: String)(implicit parser: CssParsers) = select(input)(parser)
}

// CSS 2 selectors: http://www.w3.org/TR/CSS2/selector.html
trait CssParsers extends RegexParsers {

  type Transform = NodeSeq => NodeSeq
  type Combinator = (Transform, Transform) => Transform
  type AttrComparison = (String, String) => Boolean

  override def skipWhitespace = false

  private val identity = """[A-Za-z0-9_\-]+""".r

  private val element = """[A-Za-z0-9]+""".r

  protected def noAtoms(nodes: NodeSeq) = nodes.filter(!_.isAtom)

  protected def singleToSeq(node: Node) = NodeSeq fromSeq (Seq(node))

  protected def sameLevel(node: Node)(op: Transform): NodeSeq = {
    op(node.child) ++ node.child.flatMap(sameLevel(_)(op))
  }

  protected def typeQuery(full: Boolean, query: String): Transform = {
    if (full) _ \\ query else _ \ query
  }

  private def nthGenerator(step: Int, adjust: Int): Transform = nodes => {
    if (step <= 0 && adjust == 0) nodes
    else if (step <= 0 && adjust != 0) {
      noAtoms(nodes).lift(math.abs(adjust) - 1)
        .map(singleToSeq).getOrElse(NodeSeq.Empty)
    } else {
      val interval = math.abs(step)
      val index = if (adjust <= 0) interval + adjust else (adjust - 1)
      noAtoms(nodes).sliding(interval, interval).map(ns =>
        ns.lift(index).map(singleToSeq).getOrElse(NodeSeq.Empty)
      ).foldRight(NodeSeq.Empty)(_ ++ _)
    }
  }

  def isNumber = """\d+""".r ^^ (_.toInt)

  def isSignedNumber = opt("+" | "-") ~ isNumber ^^ {
    case Some("+") ~ num => num
    case Some("-") ~ num => num * -1
    case None ~ num => num
  }

  def pseudoAccess: Parser[Transform] =
    opt(isSignedNumber <~ "n") ~ isSignedNumber ^^ {
      case Some(step) ~ adjust => nthGenerator(step, adjust)
      case None ~ adjust => nthGenerator(0, adjust)
    }

  def byUniversal: Parser[Transform] = "*" ^^ (_ =>
    typeQuery(true, "_")
  )

  def byType: Parser[Transform] = element ^^ (elem =>
    typeQuery(true, elem)
  )

  def byId: Parser[Transform] = "#" ~> identity ^^ {
    id => _ filter (n => (n \ "@id").text == id)
  }

  def byClass: Parser[Transform] = "." ~> identity ^^ {
    clazz => _ filter (n =>
      (n \ "@class").text.split("""\s+""").find(_ == clazz).isDefined
    )
  }

  def attrEqual: Parser[AttrComparison] = "=" ^^ { _ =>
    (first, second) => first.equalsIgnoreCase(second)
  }

  def attrSpaceEqual: Parser[AttrComparison] = "~=" ^^ { _ =>
    (first, second) => first.trim.split("""\s+""").find(_ == second).isDefined
  }

  def attrHyphenEqual: Parser[AttrComparison] = "|=" ^^ { _ =>
    (first, second) => first.startsWith(second + "-")
  }

  def attrStartsWith: Parser[AttrComparison] = "^=" ^^ { _ =>
    (first, second) => first.startsWith(second)
  }

  def attrContains: Parser[AttrComparison] = "*=" ^^ { _ =>
    (first, second) => first.contains(second)
  }

  def attrEndsWith: Parser[AttrComparison] = "$=" ^^ { _ =>
    (first, second) => first.endsWith(second)
  }

  def attrMatchers = (attrEqual | attrSpaceEqual | attrHyphenEqual)

  def byAttr: Parser[Transform] =
    "[" ~> identity ~ opt(attrMatchers ~ identity) <~ "]" ^^ {
      case id ~ optional =>
        val first: Transform = _ filter (_.attribute(id).isDefined)
        optional match {
          case Some(matcher ~ value) =>
            val filterOn = matcher(_: String, value)
            val second: Transform = (nodes) => {
              nodes filter (_.attribute(id)
                .map(_.map(_.text).mkString(" "))
                .filter(filterOn).isDefined
              )
            }
            first andThen second
          case None => first
        }
    }

  def byFirst: Parser[Transform] =
    (":first-child" | ":first-of-type") ^^ { _ =>
      nthGenerator(0, 1)
    }

  def byLast: Parser[Transform] =
    (":last-child" | ":last-of-type") ^^ { _ =>
      ((_: NodeSeq).reverse) andThen nthGenerator(0, 1)
    }

  def byNth: Parser[Transform] =
    (":nth-child(" ~> pseudoAccess <~ ")") |
    (":nth-of-type(" ~> pseudoAccess <~ ")")

  def byNthLast: Parser[Transform] =
    (":nth-last-child(" ~> pseudoAccess <~ ")") |
    (":nth-last-of-type(" ~> pseudoAccess <~ ")") ^^ {
      fun => ((_: NodeSeq).reverse) andThen fun
    }

  def byChild: Parser[Combinator] = """\s*>\s*""".r ^^ { _ =>
    (left, right) => nodes => (left(nodes)) flatMap { node =>
      right(node).filter(n => node.child.contains(n))
    }
  }

  def byDescend: Parser[Combinator] = whiteSpace ^^ { _ =>
    (left, right) => ns => right((left(ns)) flatMap (_.child))
  }

  def byAdjacent: Parser[Combinator] = """\s*\+\s*""".r ^^ { _ =>
    (left, right) => nodes => {
      val fNodes = left(nodes)
      nodes flatMap (sameLevel(_) { children =>
        noAtoms(children).sliding(2, 1).filter { ns =>
          val sNodes = right(ns.tail)
          ns.head == fNodes.head && !sNodes.isEmpty
        }.foldRight(NodeSeq.Empty)(_.tail ++ _)
      })
    }
  }

  def byGeneral: Parser[Combinator] = """\s*~\s*""".r ^^ { _ =>
    (left, right) => nodes => {
      val fNodes = left(nodes)
      nodes flatMap (sameLevel(_) { children =>
        val sNodes = right(children)
        if (fNodes.isEmpty || sNodes.isEmpty) NodeSeq Empty
        else {
          val first = children.indexOf(fNodes.head)
          if (first > -1 && first < children.indexOf(sNodes.head)) sNodes
          else NodeSeq Empty
        }
      })
    }
  }

  def byNegationArg = (
    byType | byUniversal | byId | byClass | byAttr | byPseudo
  )

  def byNeg: Parser[Transform] = ":not(" ~> byNegationArg <~ ")" ^^ {
    fun => nodes => {
      val selected = fun(nodes)
      nodes.filterNot(node => selected.contains(node))
    }
  }

  def leftSide = (byType | byUniversal)

  def rightSide = (byId | byClass | byAttr | byPseudo | byNeg)

  def weightedRight = (opt(leftSide) ~ rep1(rightSide)) ^^ {
    case el ~ ops => (el.getOrElse(typeQuery(true, "_")) /: ops)(_ andThen _)
  }

  def weightedLeft = (leftSide ~ rep(rightSide)) ^^ {
    case el ~ ops => (el /: ops)(_ andThen _)
  }

  def bySimpleSelect = (weightedLeft | weightedRight)

  def byCombinator = (byAdjacent | byGeneral | byChild | byDescend)

  def byPseudo = (byFirst | byLast | byNth | byNthLast)

  def selectorSeq: Parser[Transform] =
    bySimpleSelect ~ rep(byCombinator ~ bySimpleSelect) ^^ {
      case select ~ selects => (select /: selects)({
        case (left, combo ~ right) => combo(left, right)
      })
    }

  def selectorGroup: Parser[Transform] = rep1sep(selectorSeq, """\s*,\s*""".r) ^^ {
    funs => (nodes) => funs match {
      case h :: Nil => h.apply(nodes)
      case ls => ls.map(_.apply(nodes)).reduceLeft(_ ++ _)
    }
  }

  def apply(input: String) = parseAll(selectorGroup, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => throw new CssSelectorException(failure.msg)
  }
}

class CssSelectorException(message: String) extends RuntimeException(message)
