name := "regsym"

scalaVersion := "2.13.2"

libraryDependencies ++=
  "org.scalacheck" %% "scalacheck" % "1.14.3" % "test" ::
  "org.typelevel" %% "claimant" % "0.1.3" % "test" ::
  Nil

testOptions in Test +=
  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1")
