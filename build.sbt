import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "technologies.fabernovel.com",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "ScalaIO2017-ESMonad",
    libraryDependencies += scalaTest % Test
  )
