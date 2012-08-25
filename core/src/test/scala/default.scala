package css.query
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class DefaultTest extends FlatSpec with ShouldMatchers {
  "Default import" should "import standard implicit behavior" in {
    import default._

    val html =
<html>
  <head><title>Test</title></head>
  <body>
    <div id="pages">
      <div class="page">
        <div class="header"><h2>Page One</h2></div>
        <div class="body"><p>Here we go!</p></div>
      </div>
    </div>
  </body>
</html>

    (html $ "div h2").text should be === "Page One"
    (html ? ".body p") map (_.text) should be === Some("Here we go!")
    (html ?? "^creedhead^") should be ('left)
  }
}
