# CSS Query

_CSS Query_ is a Scala library to filter XML literals using CSS selectors. The
library currently supports CSS2 and CSS3 selectors.

## Examples

``` scala
import css.query._

val html =
<html>
  <head>
    <title>Test</title>
  </head>
  <body>
    <div id="content">
      <ul class="nav">
        <li>One</li>
        <li>Two</li>
    </div>
  </body>
</html>

(html $ "#content li").map(_.text).foreach(println)

// One
// Two
```

## License

MIT, 2012 Philip Cali
