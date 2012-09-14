import sbt._
import Keys._

object MyBuild extends Build {
  val defaultSettings = Defaults.defaultSettings ++ Seq (
    version := "0.1.0",
    organization := "com.github.philcali",
    scalaVersion := "2.9.2",
    crossScalaVersions := Seq("2.9.2", "2.9.1", "2.8.2", "2.8.1")
  )

  val core = Project(
    "css-query-core",
    file("core"),
    settings = defaultSettings ++ Seq(
      libraryDependencies ++= Seq(
        "com.github.philcali" %% "lmxml-core" % "0.1.2" % "test",
        "org.scalatest" %% "scalatest" % "1.8" % "test"
      )
    )
  )

  val root = Project(
    "css-query",
    file("."),
    settings = defaultSettings
  ) aggregate (core)

  // Plans to add app
}
