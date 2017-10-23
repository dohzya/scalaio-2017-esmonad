import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "technologies.fabernovel.com",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "ScalaIO2017-ESMonad",
    scalacOptions += "-feature",
    scalacOptions += "-language:postfixOps",
    scalacOptions += "-Ypartial-unification",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-MF"
  )
