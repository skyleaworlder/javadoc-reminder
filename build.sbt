ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "javadoc-reminder",
    idePackagePrefix := Some("edu.fudan.selab")
  )

// https://mvnrepository.com/artifact/junit/junit
libraryDependencies += "junit" % "junit" % "4.13.2" % Test

// https://mvnrepository.com/artifact/org.soot-oss/soot
libraryDependencies += "org.soot-oss" % "soot" % "4.3.0"

// jdt
// https://mvnrepository.com/artifact/org.eclipse.jdt/org.eclipse.jdt.core
libraryDependencies += "org.eclipse.jdt" % "org.eclipse.jdt.core" % "3.31.0"
