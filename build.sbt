import sbt.Keys._


name := "abandon_reports"

version := "1.5.0"

ThisBuild / scalaVersion := "3.1.2"

ThisBuild / scalacOptions := List("-deprecation", "-feature", "-language:implicitConversions", "-language:reflectiveCalls")

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.1.0"

libraryDependencies += "org.apache.xmlgraphics" % "fop" % "2.7"

assembly / assemblyMergeStrategy := {
  case PathList("org", "w3c", "dom", xs @ _*)         ⇒ MergeStrategy.first
  case x ⇒
    val oldStrategy = (assembly / assemblyMergeStrategy ).value
    oldStrategy(x)
}

