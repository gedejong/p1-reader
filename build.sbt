name := "p1-reader"

version := "1.0"

scalaVersion := "2.11.6"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.github.nscala-time" %% "nscala-time" % "1.6.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "com.squants"  %% "squants" % "0.6.0-SNAPSHOT",
  "org.spire-math" %% "cats-state" % "0.1.0-SNAPSHOT",
  "org.spire-math" %% "cats-std" % "0.1.0-SNAPSHOT",
  "org.spire-math" %% "cats-free" % "0.1.0-SNAPSHOT",
  compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.4"))

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

logBuffered := false

parallelExecution in Test := false

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)
