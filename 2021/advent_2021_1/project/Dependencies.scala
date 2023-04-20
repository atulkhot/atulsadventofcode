import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val monocleCore = "com.github.julien-truffaut" %% "monocle-core" % "2.1.0"
  lazy val monocleMacros = "com.github.julien-truffaut" %% "monocle-macro" % "2.1.0"
  lazy val catsCore = "org.typelevel" %% "cats-core" % "2.1.1"
}
