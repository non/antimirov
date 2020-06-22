import ReleaseTransformations._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

def ScalaCheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.14.3")
def ScalaProps = Def.setting("com.github.scalaprops" %%% "scalaprops" % "0.8.0")
def Claimant = Def.setting("org.typelevel" %%% "claimant" % "0.1.3")

lazy val antimirovSettings = Seq(
  organization := "org.spire-math",
  scalaVersion := "2.13.2",
  crossScalaVersions := Seq("2.13.2", "2.12.11"),
  libraryDependencies ++=
    ScalaCheck.value % Test ::
    Claimant.value % Test ::
    Nil,
  testOptions in Test +=
    //Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1", "-w", "8"),
    Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
  scalacOptions ++=
    "-deprecation" ::
    "-encoding" :: "UTF-8" ::
    "-feature" ::
    "-language:existentials" ::
    "-language:higherKinds" ::
    "-language:implicitConversions" ::
    "-language:experimental.macros" ::
    "-unchecked" ::
    //"-Xfatal-warnings" :: // kind of brutal in 2.13
    "-Xlint" ::
    "-Ywarn-dead-code" ::
    "-Ywarn-numeric-widen" ::
    "-Ywarn-value-discard" ::
    Nil,
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 => Seq("-Xfuture")
      case _ => Nil
    }
  },
  // HACK: without these lines, the console is basically unusable,
  // since all imports are reported as being unused (and then become
  // fatal errors).
  scalacOptions in (Compile, console) ~= { _.filterNot("-Xlint" == _) },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  // release stuff
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges
  ),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("Snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("Releases" at nexus + "service/local/staging/deploy/maven2")
  },
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/non/antimirov"),
      "scm:git:git@github.com:non/antimirov.git")),
  homepage := Some(url("https://github.com/non/antimirov/")),
  licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer("non", "Erik Osheim", "erik@osheim.org", url("http://github.com/non/"))))

lazy val noPublish = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false)

lazy val root = project
  .in(file("."))
  .settings(name := "root")
  .settings(antimirovSettings: _*)
  .settings(noPublish: _*)
  .aggregate(coreJVM, coreJS, checkJVM, checkJS, propsJVM, propsJS, testsJVM, testsJS)
  .dependsOn(coreJVM, coreJS, checkJVM, checkJS, propsJVM, propsJS, testsJVM, testsJS)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(antimirovSettings: _*)
  .settings(
    name := "antimirov-core")
  .jsSettings(
    scalaJSStage in Global := FastOptStage,
    parallelExecution := false,
    coverageEnabled := false,
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv())

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val check = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("check"))
  .dependsOn(core)
  .settings(antimirovSettings: _*)
  .settings(
    name := "antimirov-check",
    libraryDependencies += ScalaCheck.value)
  .jsSettings(
    scalaJSStage in Global := FastOptStage,
    parallelExecution := false,
    coverageEnabled := false,
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv())

lazy val checkJVM = check.jvm
lazy val checkJS = check.js

lazy val props = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("props"))
  .dependsOn(core)
  .settings(antimirovSettings: _*)
  .settings(
    name := "antimirov-props",
    libraryDependencies += ScalaProps.value,
    // keep scalaprops testing separate so we can use parallel
    // execution for all our other tests. otherwise we'd have to run
    // all of tests sequentially.
    testFrameworks += new TestFramework("scalaprops.ScalapropsFramework"),
    parallelExecution in Test := false)
  .jsSettings(
    scalaJSStage in Global := FastOptStage,
    parallelExecution := false,
    coverageEnabled := false,
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv())

lazy val propsJVM = props.jvm
lazy val propsJS = props.js

lazy val tests = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("tests"))
  .dependsOn(core, check)
  .settings(antimirovSettings: _*)
  .settings(noPublish: _*)
  .settings(
    name := "antimirov-tests",
    libraryDependencies += ScalaCheck.value)
  .jsSettings(
    scalaJSStage in Global := FastOptStage,
    parallelExecution := false,
    coverageEnabled := false,
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv())

lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js

lazy val web = project.in(file("web"))
  .dependsOn(coreJS)
  .settings(name := "antimirov-web")
  .settings(antimirovSettings: _*)
  .settings(noPublish: _*)
  .settings(coverageEnabled := false)
  .settings(scalaJSUseMainModuleInitializer := true)
  .settings(libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0")
  .enablePlugins(ScalaJSPlugin)

lazy val bench = project.in(file("bench"))
  .dependsOn(coreJVM)
  .settings(name := "antimirov-bench")
  .settings(antimirovSettings: _*)
  .settings(noPublish: _*)
  .settings(coverageEnabled := false)
  .enablePlugins(JmhPlugin)
