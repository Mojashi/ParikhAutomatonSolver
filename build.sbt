ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "2.13.10"


githubOwner := "Mojashi"
githubRepository := "ParikhAutomatonSolver"
githubTokenSource := TokenSource.Or(
  TokenSource.Environment("GITHUB_TOKEN"), // Injected during a github workflow for publishing
  // workaround https://github.com/djspiewak/sbt-github-packages/issues/28
  TokenSource.Environment("SHELL"),  // safe to assume this will be set in all our devs environments, usually /bin/bash, doesn't matter what it is to prevent local errors
)

libraryDependencies += {
  System.getProperty("os.name").toLowerCase match {
    case mac if mac.contains("mac")  => "com.google.ortools" % "ortools-darwin-aarch64" % "9.5.2237"
    case linux if linux.contains("linux") => "com.google.ortools" % "ortools-linux-aarch64" % "9.5.2237"
  }
}

libraryDependencies += "org.sosy-lab" % "java-smt" % "3.14.3"

libraryDependencies += "com.google.ortools" % "ortools-java" % "9.5.2237"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test"

libraryDependencies += "dk.brics" % "automaton" % "1.12-4"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.10"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"

lazy val root = (project in file("."))
  .settings(
    name := "ParikhAutomatonSolver",
    idePackagePrefix := Some("xyz.mojashi")
  )
