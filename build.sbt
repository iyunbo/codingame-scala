name := "codingame-scala"

version := "1.2"

scalaVersion := "2.13.4"

idePackagePrefix := Some("org.iyunbo.coding")

scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1"
libraryDependencies += "org.typelevel" %% "jawn-parser" % "0.14.2"
libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.19"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
libraryDependencies += "org.scalatestplus" %% "selenium-3-141" % "3.2.7.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.7" % "test"
libraryDependencies += "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.7" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")

Global / lintUnusedKeysOnLoad := false

// strangely, idea uses ivy2 and does not work with coursier
ThisBuild / useCoursier := false

mainClass in(Compile, packageBin) := Some("org.iyunbo.coding.vaccine.RdvVaccine")

lazy val root = (project in file(".")).
  settings(
    assemblyJarName in assembly := s"fight-covid-${version.value}.jar"
  )