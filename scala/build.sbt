val scala3Version = "3.4.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "copilot-sdk-supercharged",
    version := "2.5.3",
    scalaVersion := scala3Version,
    organization := "io.github.jeremiahjordanisaacson",
    description := "Scala SDK for the GitHub Copilot CLI",

    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core"    % "0.14.9",
      "io.circe" %% "circe-generic" % "0.14.9",
      "io.circe" %% "circe-parser"  % "0.14.9",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    ),

    // Include e2e/ as an additional test source directory
    Test / unmanagedSourceDirectories += baseDirectory.value / "e2e",

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
    ),

    // Fork a dedicated JVM for tests. The E2E suite spawns CLI subprocesses and
    // uses blocking `Await.result` calls inside Futures on ExecutionContext.global;
    // running unforked in the sbt JVM let ForkJoinPool compensation threads and
    // leaked processes accumulate until the CI runner was reclaimed mid-run. A
    // forked, memory-bounded JVM isolates that work so the runner stays healthy.
    fork := true,
    Test / fork := true,
    Test / javaOptions ++= Seq("-Xmx3g"),
  )
