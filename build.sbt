import sbt.Keys._


name := "abandon_reports"

version := "1.2"

scalaVersion in ThisBuild := "2.12.4"

scalacOptions in ThisBuild := List("-deprecation", "-feature", "-language:implicitConversions", "-language:reflectiveCalls")

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.1.0"

libraryDependencies += "org.apache.xmlgraphics" % "fop" % "2.2"

assemblyMergeStrategy in assembly := {                                                                                                   
  case PathList("org", "w3c", "dom", xs @ _*)         ⇒ MergeStrategy.first
  case x ⇒
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

