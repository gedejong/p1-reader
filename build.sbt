name := "p1-reader"

version := "1.0"

scalaVersion := "2.11.6"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "org.spire-math" %% "cats-core" % "0.1.0-SNAPSHOT",
  "org.spire-math" %% "cats-state" % "0.1.0-SNAPSHOT",
  "org.spire-math" %% "cats-std" % "0.1.0-SNAPSHOT",
  "org.spire-math" %% "cats-free" % "0.1.0-SNAPSHOT",
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.4"))

logBuffered := false

parallelExecution in Test := false

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8")
