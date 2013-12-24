// build.sbt for the scala-c3d project.
//
// Note: This file relies upon build/Common.scala for common settings.
// Each sub-project is defined below as a lazy val.
// Also defined is the fetch-c3d-example-files task, which fetches example
// files from c3d.org for testing purposes.

lazy val c3d = project settings (
  version := Common.version,
  scalaVersion := Common.buildScalaVersion,
  libraryDependencies ++= List(
    Common.scalareflect,
    Common.scalaz,
    Common.scalatest,
    Common.ejml
  ),
  parallelExecution in test := false   // SI-6240; should be fixed in Scala 2.11
)

//-----------------------------------------------------------------------------

lazy val c3d2xml = project settings (
  version := Common.version,
  scalaVersion := Common.buildScalaVersion,
  libraryDependencies ++= List(
    Common.scallop
  ),
  parallelExecution in Test := false   // SI-6240; should be fixed in Scala 2.11
) dependsOn (
  c3d
)

//-----------------------------------------------------------------------------

lazy val `xml-test-suite` = project settings (
  version := Common.version,
  scalaVersion := Common.buildScalaVersion,
  libraryDependencies ++= List(
    Common.scalatest,
    Common.xmlunit
  ),
  parallelExecution in Test := false   // SI-6240; should be fixed in Scala 2.11
) dependsOn (
  c3d2xml
)

//-----------------------------------------------------------------------------

lazy val `viewer` = project settings (
  version := Common.version,
  scalaVersion := Common.buildScalaVersion,
  libraryDependencies ++= List(
    Common.scalatest
  ),
  fork in run := true,
  parallelExecution in Test := false   // SI-6240; should be fixed in Scala 2.11
) dependsOn (
  c3d
)

//-----------------------------------------------------------------------------

val `fetch-c3d-example-files` = TaskKey[Unit](
  "fetch-c3d-example-files",
  "Fetches and unzips example C3D files from the c3d.org website (used for testing)"
)

`fetch-c3d-example-files` := {
  println("Fetching example C3D files...")
  val examples = Seq("sample01", "sample08")
  for (example <- examples) {
    val srcString = "http://www.c3d.org/data/%s.zip" format example
    val dstString = "./c3d.org-example-files/%s/" format example
    println("Fetching %s to %s" format (srcString, dstString))
    IO.unzipURL(new URL(srcString), new File(dstString))
  }
}
