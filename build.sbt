val scala3Version = "3.1.2"

lazy val samples = project
  .in(file("samples"))
  .settings(
    name := "amaranth-samples",
    scalaVersion := scala3Version,
  )

lazy val visualizer = project
  .in(file("visualizer"))
  .dependsOn(samples)
  .settings(
    name := "amaranth",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    artifactName :=  { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
      artifact.name + "." + artifact.extension
    },

    assembly / mainClass := Some("net.jmesyou.amaranth.Main"),
    assembly / assemblyJarName := "amaranth.jar",

    libraryDependencies ++= Seq(
      ("org.scala-graph" %% "graph-core" % "1.13.5").cross(CrossVersion.for3Use2_13),
      ("org.scala-graph" %% "graph-dot" % "1.13.3").cross(CrossVersion.for3Use2_13),
      "com.github.scopt" %% "scopt" % "4.0.1",
      "org.scala-lang" %% "scala3-tasty-inspector" % scala3Version,
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )