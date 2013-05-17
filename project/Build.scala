import sbt._
import Keys._

object BuildSettings {
  val buildName          = "c3d-scala"
  val buildVersion       = "0.1-SNAPSHOT"
  val buildScalaVersion  = "2.10.1"
  val buildScalacOptions = Seq.empty[String]

  val buildSettings = Defaults.defaultSettings ++ Seq(
    name          := buildName,
    version       := buildVersion,
    scalaVersion  := buildScalaVersion,
    scalacOptions := buildScalacOptions
  )
}

object Resolvers {
  val scalaToolsSnapshots = "Scala Tools Snapshots" at
    "http://scala-tools.org/repo-snapshots/"

  val projectResolvers = Seq(scalaToolsSnapshots)
}

object Dependencies {
  val scalaz       = "org.scalaz"     %% "scalaz-core"   % "7.0.0"
  val scalareflect = "org.scala-lang" %  "scala-reflect" % "2.10.1"
  val scalatest    = "org.scalatest"  %% "scalatest"     % "2.0.M6-SNAP16" % "test"

  val projectDependencies = Seq(scalaz, scalareflect, scalatest)
}

object Tasks {
  val `fetch-c3d-example-files` = TaskKey[Unit](
    "fetch-c3d-example-files",
    "Fetches and unzips example C3D files from the c3d.org website, used for testing"
  )
  val fetchC3DTask = `fetch-c3d-example-files` := {
    println("Fetching example C3D files...")
    val examples = Seq("sample08")
    for (example <- examples) {
      val srcString = "http://www.c3d.org/data/%s.zip" format example
      val dstString = "./c3d.org-example-files/%s/" format example
      println("Fetching %s to %s" format (srcString, dstString))
      IO.unzipURL(new URL(srcString), new File(dstString))
    }
  }
  val projectTasks = Seq(fetchC3DTask)
}

object C3DScalaBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._
  import Tasks._

  lazy val c3dscala = Project(
    "c3d-scala",
    file("."),
    settings = buildSettings ++ projectTasks,
    aggregate = Seq(c3d)
  )

  lazy val c3d = Project(
    id = "c3d",
    base = file("c3d"),
    settings = buildSettings ++ Seq(
      resolvers           := projectResolvers,
      libraryDependencies ++= projectDependencies
    )
  )
}
