import sbt.Keys._


name := "abandon_reports"

version := "1.4.0"

scalaVersion in ThisBuild := "2.13.3"

scalacOptions in ThisBuild := List("-deprecation", "-feature", "-language:implicitConversions", "-language:reflectiveCalls")

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.3.0"

libraryDependencies += "org.apache.xmlgraphics" % "fop" % "2.7"

assemblyMergeStrategy in assembly := {                                                                                                   
  case PathList("org", "w3c", "dom", xs @ _*)         ⇒ MergeStrategy.first
  case x ⇒
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

