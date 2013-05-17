import sbt._
import Keys._

object CommonBuildSettings {
  val buildVersion       = "0.1-SNAPSHOT"
  val buildScalaVersion  = "2.10.1"
  val buildScalacOptions = Seq.empty[String]

  val commonBuildSettings = Defaults.defaultSettings ++ Seq(
    version       := buildVersion,
    scalaVersion  := buildScalaVersion,
    scalacOptions := buildScalacOptions
  )
}

object C3DScalaBuildSettings { // top level
  import CommonBuildSettings._
  val buildName = "scala-c3d"
  val buildSettings = commonBuildSettings ++ Seq(
    name := buildName
  )
}

object C3DBuildSettings {
  import CommonBuildSettings._
  val buildName = "c3d"
  val buildSettings = commonBuildSettings ++ Seq(
    name := buildName
  )
}

object C3D2XMLBuildSettings {
  import CommonBuildSettings._
  val buildName = "c3d2xml"
  val buildSettings = commonBuildSettings ++ Seq(
    name := buildName
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
  val scallop      = "org.rogach"     %% "scallop"       % "0.9.1"
}

object C3DDependencies {
  import Dependencies._
  val projectDependencies = Seq(scalaz, scalareflect, scalatest)
}

object C3D2XMLDependencies {
  import Dependencies._
  val projectDependencies = Seq(scalaz, scalareflect, scalatest, scallop)
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
  import Tasks._

  lazy val c3d: Project = {
    import C3DBuildSettings._
    import C3DDependencies._
    Project(
      id = "c3d",
      base = file("c3d"),
      settings = buildSettings ++ Seq(
        resolvers           :=  projectResolvers,
        libraryDependencies ++= projectDependencies
      )
    )
  }

  lazy val c3d2xml: Project = {
    import C3D2XMLBuildSettings._
    import C3D2XMLDependencies._
    Project(
      id = "c3d2xml",
      base = file("c3d2xml"),
      settings = buildSettings ++ Seq(
        resolvers           :=  projectResolvers,
        libraryDependencies ++= projectDependencies
      )
    ) dependsOn (c3d)
  }

  lazy val c3dscala = {
    import C3DScalaBuildSettings._
    Project(
      id = "c3d-scala",
      base = file("."),
      settings = buildSettings ++ projectTasks
    ) aggregate(c3d, c3d2xml)
  }

}
