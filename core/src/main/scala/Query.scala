package css.query

import util.parsing.combinator.RegexParsers
import java.lang.RuntimeException

import xml.{ NodeSeq, Node }

import util.control.Exception.allCatch

trait CssImplicits {
  implicit def nodeSeqToCssSelect(nodes: NodeSeq) = RichCssSelection(nodes)
}

case class RichCssSelection(origin: NodeSeq) {
  def select(input: String)(implicit parser: CssParsers) = parser(input)(origin)

  def selectOption(input: String)(implicit parser: CssParsers) = {
    allCatch.opt(select(input)(parser)) filter (!_.isEmpty)
  }

  def selectEither(input: String)(implicit parser: CssParsers) = {
    allCatch.either(select(input)(parser))
  }

  def $(input: String)(implicit parser: CssParsers) = select(input)(parser)

  def ?(input: String)(implicit parser: CssParsers) = selectOption(input)(parser)

  def ??(input: String)(implicit parser: CssParsers) = selectEither(input)(parser)
}

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

  private def nthGenerator(step: Int, adjust: Int): Transform = nodes => {
    if (step <= 0 && adjust == 0) nodes
    else if (step <= 0 && adjust != 0) {
      nodes.lift(math.abs(adjust) - 1).map(singleToSeq).getOrElse(NodeSeq.Empty)
    } else {
      val interval = math.abs(step)
      val index = if (adjust <= 0) interval + adjust else (adjust - 1)
      nodes.sliding(interval, interval).map(ns =>
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

  def byUniversal: Parser[Transform] = "*" ^^ (_ => _ filter (!_.isAtom))

  def byType: Parser[Transform] = element ^^ (elem =>
    _ filter (_.label.equalsIgnoreCase(elem))
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

  def attrMatchers = (
    attrEqual | attrSpaceEqual | attrHyphenEqual |
    attrStartsWith | attrContains | attrEndsWith
  )

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
    (left, right) => nodes => left(nodes) flatMap (node => right(node.child))
  }

  def byDescend: Parser[Combinator] = whiteSpace ^^ { _ =>
    (left, right) => nodes =>
      left(nodes) flatMap { node =>
        val children = node.child
        val sameSelect = left(children)
        val valid = children.filterNot(sameSelect.contains)
        right(valid) ++ (valid flatMap (sameLevel(_)(right)))
      }
  }

  def byAdjacent: Parser[Combinator] = """\s*\+\s*""".r ^^ { _ =>
    (left, right) => nodes => {
      noAtoms(nodes).sliding(2, 1).flatMap { ns =>
        val fNodes = left(ns.head)
        val sNodes = right(ns.tail)
        if (fNodes.isEmpty || sNodes.isEmpty) NodeSeq Empty
        else sNodes
      }.toList
    }
  }

  def byGeneral: Parser[Combinator] = """\s*~\s*""".r ^^ { _ =>
    (left, right) => nodes => {
      val fNodes = left(nodes)
      if (fNodes.isEmpty) NodeSeq Empty
      else {
        val first = nodes.indexOf(fNodes.head)
        if (first == -1) NodeSeq Empty else right(nodes.splitAt(first)._2)
      }
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

  def weightedRight: Parser[Transform] = (opt(leftSide) ~ rep1(rightSide)) ^^ {
    case el ~ ops =>
      (el.getOrElse((_: NodeSeq).filter(!_.isAtom)) /: ops)(_ andThen _)
  }

  def weightedLeft: Parser[Transform] = (leftSide ~ rep(rightSide)) ^^ {
    case el ~ ops => (el /: ops)(_ andThen _)
  }

  def bySimpleSelect = (weightedLeft | weightedRight)

  def byCombinator = (byAdjacent | byGeneral | byChild | byDescend)

  def byPseudo = (byFirst | byLast | byNth | byNthLast)

  def selectorSeq: Parser[Transform] =
    bySimpleSelect ~ rep(byCombinator ~ bySimpleSelect) ^^ {
      case select ~ selects =>
        val combinators = selects.reverse match {
          case (hcombo ~ hright) :: rest =>
            rest.foldLeft(hcombo(_: Transform, hright))({
              case (application, combo ~ right) => combo(_, application(right))
            }).apply(select)
          case Nil => select
        }
        _ flatMap (node => combinators(node) ++ sameLevel(node)(combinators))
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
