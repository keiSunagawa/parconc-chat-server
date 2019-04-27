import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "me.kerfume"

lazy val root = (project in file("."))
  .settings(
    name := "chat-server",
    trapExit := false,
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scalaz" %% "scalaz-zio" % "1.0-RC4"
  )
