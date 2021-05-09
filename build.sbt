name := "codingame-scala"

version := "0.1"

scalaVersion := "2.13.4"

idePackagePrefix := Some("org.iyunbo.coding")

scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1"
libraryDependencies += "org.typelevel" %% "jawn-parser" % "0.14.2"
libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.19"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")

Global / lintUnusedKeysOnLoad := false