import sbt._
import Keys._

object MyBuild extends Build {
  val defaultSettings = Defaults.defaultSettings ++ Seq (
    version := "0.1.0",
    organization := "com.github.philcali",
    scalaVersion := "2.9.2",
    crossScalaVersions := Seq("2.9.2", "2.9.1", "2.8.2", "2.8.1"),
    publishTo <<= version { v =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>https://github.com/philcali/css-query</url>
      <licenses>
        <license>
          <name>The MIT License</name>
          <url>http://www.opensource.org/licenses/mit-license.php</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:philcali/css-query.git</url>
        <connection>scm:git:git@github.com:philcali/css-query.git</connection>
      </scm>
      <developers>
        <developer>
          <id>philcali</id>
          <name>Philip Cali</name>
          <url>http://philcalicode.blogspot.com/</url>
        </developer>
      </developers>
    )
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
