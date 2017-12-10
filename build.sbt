import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-RC1",
    libraryDependencies += "org.typelevel" %% "cats-free" % "1.0.0-RC1",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "0.5",
    libraryDependencies += "org.scodec" %% "scodec-bits" % "1.1.5",
    libraryDependencies += "commons-io" % "commons-io" % "2.6",
    scalacOptions += "-Ypartial-unification"
  )
