name := "Astronomy"

organization := "org.fathens"

version := "1.1.3"

scalaVersion := "2.11.5"

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:reflectiveCalls",
  "-feature"
)

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= Seq(
  "org.fathens" %% "math-core" % "0.1.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "org.specs2" %% "specs2-scalacheck" % "2.4.15" % "test"
)
