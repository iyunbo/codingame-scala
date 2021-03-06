name := "codingame-scala"

version := "0.1"

scalaVersion := "2.13.4"

idePackagePrefix := Some("org.iyunbo.coding")

scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")

Global / lintUnusedKeysOnLoad := false