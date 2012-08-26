package css.query
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import lmxml.{ PlainLmxmlFactory, Conversion, XmlConvert, XmlFormat }

class CssQueryTest extends FlatSpec with ShouldMatchers with CssImplicits {
  implicit object CssSelectors extends CssParsers

  object Lmxml extends PlainLmxmlFactory with Conversion

  val source = """
html @lang="en"
  head title "This is a test"
  body
    div#page
      div#header h1 "Test Page"
      div#nav
        ul.navigator
          li "Home"
          li "About"
      div.main
        div.pages
          div.page
            div.header h3 "Page One"
            div.body
              p "Here's some stuff"
              ul
                li "One"
                li "Two"
              span "ain't it great?"
          div.page.last
            div.header h3 "Page Two"
            div.body
              p "Here's some more stuff"
              ol
                li "One"
                li strong "Two"
"""
  val html = Lmxml.convert(source)(XmlConvert)

  "CSS2 selectors" should "select by id" in {
    ((html $ "#header") \ "h1").text should be === "Test Page"
  }

  it should "select by class contains" in {
    (html $ ".page").size should be === 2
  }

  it should "select by descendants" in {
    (html $ ".page h3").text should be === "Page OnePage Two"
  }

  it should "select by child selectors" in {
    (html $ ".header > h3").text should be === "Page OnePage Two"
  }

  it should "select by first child pseudo class" in {
    (html $ ".pages .page:first-child h3").text should be === "Page One"
  }

  it should "support adjacent selectors" in {
    (html $ ".page:first-child div + div > p").text should be === "Here's some stuff"
  }

  it should "support attribute selection" in {
    (html $ "div[class~=last] p").text should be === "Here's some more stuff"
  }

  it should "support attribute equals selection" in {
    (html $ "div[id=page]").head.label should be === "div"
  }

  "CSS3 selectors" should "select pseudo class of last child" in {
    (html $ ".page:last-child") should be === (html $ ".page.last")
    (html $ "ul > li:last-child").text should be === "AboutTwo"
  }

  it should "support the negation pseudos" in {
    (html $ "div:not(.page):not(.body)").size should be === 7
    (html $ "ul > li:not(:last-child)").text should be === "HomeOne"
  }

  it should "support general precedance" in {
    (html $ "p ~ span").text should be === "ain't it great?"
  }

  it should "support attribute contains" in {
    (html $ "div[class*=age]").size should be === 3
  }

  it should "support attribute starts with" in {
    (html $ "div[class^=mai]").size should be === 1
  }

  it should "support attribute ends with" in {
    (html $ "div[class$=der]").size should be === 2
  }

  it should "support nth selection" in {
    (html $ "ul > li:nth-child(1)").text should be === "HomeOne"
  }

  "CssParsers" should "be extensible" in {
    val parser = new CssParsers {
      def byNthDescent: Parser[Combinator] =
        """\s*>>""".r ~> opt("(" ~> isNumber <~ ")") <~ """\s*""".r ^^ {
          level => (left, right) => (nodes) =>
            right((left(nodes) /: (1 to level.filter(_ >= 2).getOrElse(2)))({
              case (initialNodes, i) => initialNodes.flatMap(_.child)
            }))
        }

      override def byCombinator = (byNthDescent | super.byCombinator)
    }

    (html.select("ol >> strong")(parser)).text should be === "Two"
    (html.select(".pages >>(3) h3")(parser)).text should be === "Page OnePage Two"
  }

  it should "support options" in {
    (html ? "!blarg") should be === None
    (html ? "#nav") map (_ $ "li") should be ('defined)
  }

  it should "support eithers" in {
    evaluating (html $ "!blarg") should produce [CssSelectorException]
    (html ?? "!blarg") should be ('left)
    (html ?? "div ol li") should be ('right)
  }

  it should "facilitate reverse lookups" in {
    // Get's all p elements in [class~=body] elements
    (html $ ".body p").size should be === 2
    // Get's all [class~=body] elements which contains a p element
    ((html $ ".body") filter (_ ? "p" isDefined)).size should be === 2
  }
}
