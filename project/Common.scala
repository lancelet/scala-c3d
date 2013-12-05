import sbt._
import Keys._

object Common {

  // version of scala to be used
  val version = "0.1-SNAPSHOT"
  val buildScalaVersion = "2.10.3"

  // dependencies
  val scalareflect = "org.scala-lang" %  "scala-reflect" % "2.10.3"
  val scalaz       = "org.scalaz"     %% "scalaz-core"   % "7.0.5"
  val scalatest    = "org.scalatest"  %% "scalatest"     % "2.0" % "test"
  val scallop      = "org.rogach"     %% "scallop"       % "0.9.4"
  val xmlunit      = "xmlunit"        %  "xmlunit"       % "1.5"
  val ejml         = "com.googlecode.efficient-java-matrix-library" % "ejml" % "0.23"

}
