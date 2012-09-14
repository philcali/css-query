# CSS Query

_CSS Query_ is a Scala library to filter XML literals using CSS selectors. The
library currently supports CSS2 and CSS3 selectors.

A lot of useful information about how to use the library can be found at the
[wiki](https://github.com/philcali/css-query/wiki).

## Dependency

``` scala
libraryDependencies += "com.github.philcali" %% "css-query" % "0.1.0"
```

## Usage

``` scala
import css.query.default._

// if html was some xml.NodeSeq
(html $ "#main > .header") map (_.text) foreach println
```

## Extending

Define your module:

``` scala
package my.css

import css.query._

trait CustomCombinators extends CssParsers {
  def byNthDescent: Parser[Combinator] =
    """\s*>>""".r ~> opt("(" ~> isNumber <~ ")") <~ """\s*""".r ^^ { level =>
      (left, right) => (nodes) =>
        right((left(nodes) /: (1 to level.filter(_ >= 2).getOrElse(2))) {
          case (initialNodes, i) => initialNodes.flatMap(_.child)
        })
    }

  override def byCombinators = (byNthDescent | super.byCombinators)
}

object module extends CssImplicits {
  implicit object MyCssParser extends CssParsers with CustomCombinators
}
```

Use your module:

``` scala
import my.css.module._

(html ? "div >> p") foreach println
(html $ "div >>(3) ul") foreach println
```

## License

MIT, 2012 Philip Cali
