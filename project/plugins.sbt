addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "1.1.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
// strangely, idea uses ivy2 and does not work with coursier
ThisBuild / useCoursier := false