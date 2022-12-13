ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

libraryDependencies += {
  System.getProperty("os.name").toLowerCase match {
    case mac if mac.contains("mac")  => "com.google.ortools" % "ortools-darwin-aarch64" % "9.5.2237"
    case linux if linux.contains("linux") => "com.google.ortools" % "ortools-linux-aarch64" % "9.5.2237"
  }
}
libraryDependencies += "com.google.ortools" % "ortools-java" % "9.5.2237"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test"


lazy val root = (project in file("."))
  .settings(
    name := "ParikhAutomatonSolver",
    idePackagePrefix := Some("xyz.mojashi")
  )
