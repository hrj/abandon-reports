import sbt.Keys._


name := "abandon_reports"

version := "1.4.0"

ThisBuild / scalaVersion := "3.1.2"

scalacOptions in ThisBuild := List("-deprecation", "-feature", "-language:implicitConversions", "-language:reflectiveCalls")

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.1.0"

libraryDependencies += "org.apache.xmlgraphics" % "fop" % "2.7"

assemblyMergeStrategy in assembly := {                                                                                                   
  case PathList("org", "w3c", "dom", xs @ _*)         ⇒ MergeStrategy.first
  case x ⇒
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

