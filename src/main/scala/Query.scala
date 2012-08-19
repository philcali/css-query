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
  type AttrComparison = (String, String) => Boolean

  private val identity = """[A-Za-z0-9_\-]+""".r

  private val element = """[A-Za-z0-9]+""".r

  protected def rebuild(nodes: NodeSeq)(cond: Node => Boolean) =
    nodes flatMap (_.descendant_or_self) filter cond

  def isElem = opt(element) ^^ {
    elem => (node: Node) => elem.map(_ == node.label).getOrElse(true)
  }

  def byElement(includeSelf: Boolean): Parser[Transform] = element ^^ {
    elem =>
      if (includeSelf) _ \\ elem
      else _ flatMap (_.descendant filter (_.label == elem))
  }

  def byId: Parser[Transform] = isElem ~ "#" ~ identity ^^ {
    case check ~ "#" ~ id =>
      rebuild(_)(n => check(n) && (n \ "@id").text == id)
  }

  def byClass: Parser[Transform] = isElem ~ "." ~ identity ^^ {
    case check ~ "." ~ clazz =>
      rebuild(_)(n =>
        check(n) &&
        (n \ "@class").text.split("""\s+""").find(_ == clazz).isDefined
      )
  }

  def byDescend: Parser[Transform] = ">" ~> bySingle ^^ {
    transform => _ flatMap (_.descendant_or_self flatMap transform)
  }

  def attrEqual: Parser[AttrComparison] = "=" ^^ { _ =>
    (first, second) => first == second
  }

  def attrSpaceEqual: Parser[AttrComparison] = "~=" ^^ { _ =>
    (first, second) => first.trim.split("""\s+""").find(_ == second).isDefined
  }

  def attrHyphenEqual: Parser[AttrComparison] = "|=" ^^ { _ =>
    (first, second) => first.startsWith(second + "-")
  }

  def attrMatchers = (attrEqual | attrSpaceEqual | attrHyphenEqual)

  def byAttr: Parser[Transform] =
    isElem ~ "[" ~ identity ~ opt(attrMatchers ~ identity) <~ "]" ^^ {
      case check ~ "[" ~ id ~ optional =>
        val first: Transform = rebuild(_)(n =>
          check(n) && n.attribute(id).isDefined
        )
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

  def byFirstChild: Parser[Transform] = ":first-child" ^^ { _ =>
    _.filter(!_.isAtom).headOption match {
      case Some(node) => NodeSeq fromSeq (Seq(node))
      case None => NodeSeq Empty
    }
  }

  def bySingle = (byAttr | byClass | byId | byElement(false))

  def byAdjacent: Parser[Transform] = bySingle ~ "+" ~ bySingle ^^ {
    case first ~ "+" ~ second => (nodes) => {
      val firstNodes = first(nodes)
      val secondNodes = second(nodes)
      nodes.flatMap(_.child.filter(!_.isAtom).sliding(2, 1).filter { ns =>
        ns.head == firstNodes.head &&
        ns.tail.head == secondNodes.tail.head
      }.foldRight (NodeSeq.Empty)(_.tail ++ _))
    }
  }

  def bySelectors = (byAdjacent | byDescend | bySingle | byFirstChild)

  def allSelectors: Parser[Transform] =
    opt(bySingle | byElement(true)) ~ rep(bySelectors) ^^ {
      case None ~ selects if selects.isEmpty => (nodes) => nodes
      case elem ~ selects =>
        val funs = elem.map(_ :: selects).getOrElse(selects)
        funs.reduceLeftOption(_ andThen _).getOrElse(funs.head)
    }

  def selections: Parser[Transform] = rep1sep(allSelectors, ",") ^^ { funs =>
    (nodes) => funs.map(_.apply(nodes))
      .reduceLeftOption(_ ++ _)
      .getOrElse(funs.head.apply(nodes))
  }

  def apply(input: String) = parseAll(selections, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => throw new CssSelectorException(failure.msg)
  }
}

class CssSelectorException(message: String) extends RuntimeException(message)
