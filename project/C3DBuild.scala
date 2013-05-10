import sbt._
import Keys._

object BuildSettings {
  val buildName         = "scala-c3d"
  val buildVersion      = "0.1-SNAPSHOT"
  val buildScalaVersion = "2.10.1"

  val buildSettings = Defaults.defaultSettings ++ Seq(
    name         := buildName,
    version      := buildVersion,
    scalaVersion := buildScalaVersion
  ) ++ 
  org.scalastyle.sbt.ScalastylePlugin.Settings
}

object Resolvers {
  val scalaToolsSnapshots = "Scala Tools Snapshots" at
    "http://scala-tools.org/repo-snapshots/"

  val projectResolvers = Seq(scalaToolsSnapshots)
}

object Dependencies {
  val scalaz       = "org.scalaz"     %% "scalaz-core"   % "6.0.4"
  val scalaarm     = "com.jsuereth"   %% "scala-arm"     % "1.3"
  val scalatest    = "org.scalatest"  %% "scalatest"     % "2.0.M6-SNAP16" % "test"

  val projectDependencies = Seq(scalaz, scalaarm, scalatest)
}

object Tasks {
  val `fetch-c3d-example-files` = TaskKey[Unit](
    "fetch-c3d-example-files",
    "Fetches and unzips example C3D files from the c3d.org website for testing")
  val fetchC3DTask = `fetch-c3d-example-files` := {
    println("Fetching example C3D files...")
    val examples = Seq("sample08")
    for (example <- examples) {
      val srcString = "http://www.c3d.org/data/%s.zip" format example
      val dstString = "./resources/%s/" format example
      println("Fetching %s to %s" format (srcString, dstString))
      IO.unzipURL(new URL(srcString), new File(dstString))
    }
  }

  val projectTasks = Seq(fetchC3DTask)
}

object C3DBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._
  import Tasks._

  lazy val c3dscala = Project (
    "c3d-scala",
    file("."),
    settings = buildSettings ++ Seq(
      resolvers           :=  projectResolvers,
      libraryDependencies ++= projectDependencies
    ) ++ projectTasks
  )
}
