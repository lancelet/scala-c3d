name := "scala-c3d"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.0"

// Test library dependencies
libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

// Scalac Options
scalacOptions ++= Seq(
  "-Yno-adapted-args", 
  "-Ywarn-all", 
  "-Xfatal-warnings",
  "-deprecation",
  "-feature"
)