import sbt.Keys._

enablePlugins(NativeImagePlugin)

nativeImageOptions ++= List("--no-fallback", "--report-unsupported-elements-at-runtime", "-O2")

nativeImageJvm := "graalvm-java17"

nativeImageVersion := "22.1.0"

nativeImageOptions ++= List(
  s"-H:ConfigurationFileDirectories=${target.value / "native-image-configs" }",
  "-H:+JNI"
)

nativeImageAgentMerge := true

name := "abandon_reports"

version := "1.5.1"

ThisBuild / scalaVersion := "3.4.2"

ThisBuild / scalacOptions := List("-deprecation", "-feature", "-language:implicitConversions", "-language:reflectiveCalls")

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.3.0"

libraryDependencies += "org.apache.xmlgraphics" % "fop" % "2.7"

assembly / assemblyMergeStrategy := {
  case PathList("org", "w3c", "dom", xs @ _*)         ⇒ MergeStrategy.first
  case x ⇒
    val oldStrategy = (assembly / assemblyMergeStrategy ).value
    oldStrategy(x)
}

