addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "1.1.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0")
// strangely, idea uses ivy2 and does not work with coursier
ThisBuild / useCoursier := false