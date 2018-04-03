name := "chemodan"

version := "1.0"

scalaVersion := "2.11.8"

showSuccess := false

logLevel in run := Level.Warn

onLoadMessage := ""

libraryDependencies ++= Seq(
  "org.scala-lang.modules"  %% "scala-parser-combinators" % "1.0.4",
  "org.rogach"              %% "scallop"                  % "3.1.2"
)
