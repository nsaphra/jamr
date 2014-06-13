import AssemblyKeys._

assemblySettings

name := "jamr"

version := "0.1-SNAPSHOT"

organization := "edu.cmu.lti.nlp"

scalaVersion := "2.10.3"

//libraryDependencies ++= Seq(
//  "org.scala-lang" % "scala-swing" % "2.10.3"
//)

scalaSource in compile := (baseDirectory in compile).value  / "src"

// Running JAMR via sbt:

fork in run := true  // run in separate JVM than sbt

connectInput in run := true

outputStrategy in run := Some(StdoutOutput)  // connect to stdin/stdout/stderr of sbt's process

logLevel in run := Level.Error  // don't clutter stdout with [info]s

ivyLoggingLevel in run := UpdateLogging.Quiet

traceLevel in run := 0

javaOptions in run ++= Seq(
  "-Xmx4g",
  "-XX:MaxPermSize=256m",
  "-ea",
  "-Dfile.encoding=UTF-8",
  "-XX:ParallelGCThreads=2"
)

mainClass := Some("edu.cmu.lti.nlp.amr.AMRParser")
