package css.query
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import lmxml.{ PlainLmxmlFactory, Conversion, XmlConvert }

class CssQueryTest extends FlatSpec with ShouldMatchers {
  object TestImplicits extends CssImplicits

  implicit object CssSelectors extends CssParsers

  import TestImplicits._

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
                li "Two"
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

  it should "support nth selection" in {
    (html $ "ul > li:nth-child(1)").text should be === "HomeOne"
  }
}
