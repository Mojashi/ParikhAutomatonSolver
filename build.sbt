ThisBuild / version := "0.5.7"

ThisBuild / scalaVersion := "2.13.10"

publishTo := Some("GitHub Apache Maven Packages" at "https://maven.pkg.github.com/Mojashi/ParikhAutomatonSolver")
publishMavenStyle := true
credentials += Credentials(
  "GitHub Package Registry",
  "maven.pkg.github.com",
  "Mojashi",
  System.getenv("GITHUB_TOKEN")
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
    idePackagePrefix := Some("com.github.Mojashi")
  )
