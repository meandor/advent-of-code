import Dependencies._

ThisBuild / scalaVersion := "2.13.3"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.github.meandor"
ThisBuild / organizationName := "meandor"
ThisBuild / scapegoatVersion := "1.4.6"

lazy val root = (project in file("."))
  .settings(
    name := "day4",
    libraryDependencies += scalaTest % Test
  )
