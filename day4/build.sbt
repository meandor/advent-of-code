import Dependencies._

ThisBuild / scalaVersion := "2.13.4"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.github.meandor"
ThisBuild / organizationName := "meandor"

lazy val root = (project in file("."))
  .settings(
    name := "day4",
    libraryDependencies += scalaTest % Test
  )
