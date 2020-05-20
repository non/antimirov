
lazy val sharedSettings = Seq(
  scalaVersion := "2.13.2",

  libraryDependencies ++=
    //"org.scalacheck" %% "scalacheck" % "1.15.0-SNAPSHOT" % Test ::
    "org.scalacheck" %% "scalacheck" % "1.14.3" % Test ::
    "org.typelevel" %% "claimant" % "0.1.3" % Test ::
    Nil,

  testOptions in Test +=
    Tests.Argument(
      TestFrameworks.ScalaCheck,
      "-verbosity", "1",
//      "-initialSeed", "eBdqNuA2XAob9GhQcw0UZVjKQwkrg2cdno3oLj5PeXI=",
//      "-w", "8"
    ),

  scalacOptions ++=
    "-Yrangepos" ::
//    "-deprecation" ::
    Nil)

lazy val root = project.in(file("."))
  .settings(name := "regsym")
  .settings(sharedSettings: _*)

lazy val bench = project.in(file("bench"))
  .dependsOn(root)
  .settings(name := "regsym-bench")
  .settings(sharedSettings: _*)
  .enablePlugins(JmhPlugin)
