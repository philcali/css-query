package css.query
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class CssQueryTest extends FlatSpec with ShouldMatchers {
  object TestImplicits extends CssImplicits

  implicit object CssSelectors extends CssParsers

  import TestImplicits._

  val html =
<html lang="en">
  <head>
    <title>This is a test</title>
  </head>
  <body>
    <div id="page">
      <div id="header">
        <h1>Test Page</h1>
      </div>
      <div id="nav">
        <ul class="navigator">
          <li>Home</li>
          <li>About</li>
        </ul>
      </div>
      <div class="main">
        <div class="pages">
          <div class="page">
            <div class="header"><h3>Page One</h3></div>
            <div class="body">
              <p>Here's some stuff</p>
              <ul>
                <li>One</li>
                <li>Two</li>
              </ul>
            </div>
          </div>
          <div class="page last">
            <div class="header"><h3>Page Two</h3></div>
            <div class="body">
              <p>Here's some more stuff</p>
              <ol>
                <li>One</li>
                <li>Two</li>
              </ol>
            </div>
          </div>
        </div>
      </div>
    </div>
  </body>
</html>

  "CSS2 selectors" should "select by id" in {
    ((html $ "#header") \ "h1").text should be === "Test Page"
  }

  it should "select by class contains" in {
    (html $ ".page").size should be === 2
  }

  it should "select by descendants" in {
    (html $ ".main.pages.page h3").text should be === "Page OnePage Two"
  }

  it should "select by child selectors" in {
    (html $ ".header > h3").text should be === "Page OnePage Two"
  }

  it should "select by first child pseudo class" in {
    (html $ ".pages div:first-child h3").text should be === "Page One"
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
  }
}
