name := "regsym"

scalaVersion := "2.12.8"

libraryDependencies +=
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

testOptions in Test +=
  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1")
