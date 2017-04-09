name := "ScalaOlio"

version := "1.0"

scalaVersion := "2.11.10"

val jacksonVersion = "2.8.8"

libraryDependencies ++= Seq(
  "org.scala-lang"             % "scala-reflect"                 % "2.11.10",
  "org.scalatest"              % "scalatest_2.11"                % "3.0.1"          % "test",
  "joda-time"                  % "joda-time"                     % "2.9.9",
  "org.joda"                   % "joda-convert"                  % "1.8.1",
  "org.clapper"                % "grizzled-slf4j_2.11"           % "1.3.0",
  "org.slf4j"                  % "slf4j-simple"                  % "1.7.25",
  "org.json4s"                 % "json4s-jackson_2.11"           % "3.5.1",
  "org.typelevel"              % "squants_2.11"                  % "1.2.0",
  "com.fasterxml.jackson.core" % "jackson-databind"              % jacksonVersion,
  "com.fasterxml.jackson.core" % "jackson-core"                  % jacksonVersion,
  "com.fasterxml.jackson.core" % "jackson-annotations"           % jacksonVersion)
