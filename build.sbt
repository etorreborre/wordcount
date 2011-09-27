/** Project */
name := "wordcount"

version := "1.0"

organization := "org.specs2"

scalaVersion := "2.9.1"

scalacOptions := Seq("-deprecation")

mainClass in (Compile, packageBin) := Some("essays.CountApp")

seq(oneJarSettings: _*)

artifact in oneJar <<= moduleName(Artifact(_, "app"))

/** Dependencies */
resolvers ++= Seq("snapshots-repo" at "http://scala-tools.org/repo-snapshots", 
                  "Local Maven Repository" at "file://$M2_REPO")

libraryDependencies ++= Seq(
  "org.scala-tools.testing" %% "scalacheck" % "1.9", 
  "org.scala-tools.testing" % "test-interface" % "0.5", 
  "org.scala-lang" % "scala-swing" % "2.9.1", 
  "org.specs2" %% "specs2-scalaz-core" % "6.0.1",
  "org.specs2" %% "specs2" % "1.7-SNAPSHOT",
  "org.hamcrest" % "hamcrest-all" % "1.1",
  "org.mockito" % "mockito-all" % "1.8.5")
