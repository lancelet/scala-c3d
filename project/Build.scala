import sbt._
import Keys._

object CommonBuildSettings {
  val buildVersion       = "0.1-SNAPSHOT"
  val buildScalaVersion  = "2.10.2"
  val buildScalacOptions = Seq.empty[String]

  val commonBuildSettings = Defaults.defaultSettings ++ Seq(
    version                   := buildVersion,
    scalaVersion              := buildScalaVersion,
    scalacOptions             := buildScalacOptions,
    parallelExecution in Test := false  // turn off parallel execution of tests until SI-6240 is fixed
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

object XMLTestSuiteSettings {
  import CommonBuildSettings._
  val buildName = "xml-test-suite"
  val buildSettings = commonBuildSettings ++ Seq(
    name := buildName
  )
}

object FXViewBuildSettings {
  import CommonBuildSettings._
  val buildName = "fx-view"
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
  val scalalang    = "org.scala-lang" %  "scala-library" % "2.10.2"  // why is this required?
  val scalareflect = "org.scala-lang" %  "scala-reflect" % "2.10.2"
  val scalaz       = "org.scalaz"     %% "scalaz-core"   % "7.0.0"
  val scalatest    = "org.scalatest"  %% "scalatest"     % "2.0.RC1-SNAP4" % "test"
  val scallop      = "org.rogach"     %% "scallop"       % "0.9.1"
  val xmlunit      = "xmlunit"        %  "xmlunit"       % "1.4"
  val ejml         = "com.googlecode.efficient-java-matrix-library" % "ejml" % "0.23"
}

object C3DDependencies {
  import Dependencies._
  val projectDependencies = Seq(scalalang, scalareflect, scalaz, scalatest, ejml)
}

object C3D2XMLDependencies {
  import Dependencies._
  val projectDependencies = Seq(scalalang, scalareflect, scalaz, scalatest, scallop)
}

object XMLTestSuiteDependencies {
  import Dependencies._
  val projectDependencies = Seq(scalalang, scalareflect, scalaz, scalatest, scallop, xmlunit)
}

object FXViewDependencies {
  import Dependencies._
  val projectDependencies = Seq(scalalang)
}

object Tasks {
  val `fetch-c3d-example-files` = TaskKey[Unit](
    "fetch-c3d-example-files",
    "Fetches and unzips example C3D files from the c3d.org website, used for testing"
  )
  val fetchC3DTask = `fetch-c3d-example-files` := {
    println("Fetching example C3D files...")
    val examples = Seq("sample01", "sample08")
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

  lazy val xmlTestSuite = {
    import XMLTestSuiteSettings._
    import XMLTestSuiteDependencies._
    Project(
      id = "xml-test-suite",
      base = file("xml-test-suite"),
      settings = buildSettings ++ Seq(
        resolvers           := projectResolvers,
        libraryDependencies ++= projectDependencies
      )
    ) dependsOn (c3d, c3d2xml)
  }

  lazy val fxView = {
    import FXViewBuildSettings._
    import FXViewDependencies._
    Project (
      id = "fx-view",
      base = file("fx-view"),
      settings = buildSettings ++ Seq(
        resolvers           := projectResolvers,
        libraryDependencies ++= projectDependencies,
        fork in run         := true
      )
    ) dependsOn (c3d)
  }

  lazy val c3dscala = {
    import C3DScalaBuildSettings._
    Project(
      id = "c3d-scala",
      base = file("."),
      settings = buildSettings ++ projectTasks
    ) aggregate(c3d, c3d2xml, xmlTestSuite, fxView)
  }

}
