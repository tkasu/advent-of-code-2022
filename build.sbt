val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Advent of Code 2022",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq (
      "org.typelevel" %% "cats-effect" % "3.4.4",
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      )
  )
