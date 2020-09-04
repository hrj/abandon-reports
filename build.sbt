import sbt.Keys._


name := "abandon_reports"

version := "1.3.3"

scalaVersion in ThisBuild := "2.13.3"

scalacOptions in ThisBuild := List("-deprecation", "-feature", "-language:implicitConversions", "-language:reflectiveCalls")

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.3.0"

// Not able to update to version 2.4 or 2.5 because of missing transitive depedencies: jai-core and jai-codec.
// This should be fixed in the next release: https://issues.apache.org/jira/browse/FOP-2889
libraryDependencies += "org.apache.xmlgraphics" % "fop" % "2.3"

assemblyMergeStrategy in assembly := {                                                                                                   
  case PathList("org", "w3c", "dom", xs @ _*)         ⇒ MergeStrategy.first
  case x ⇒
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

