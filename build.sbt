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
libraryDependencies += "org.soot-oss" % "soot" % "4.2.1"

// jdt
// https://mvnrepository.com/artifact/org.eclipse.jdt/org.eclipse.jdt.core
libraryDependencies += "org.eclipse.jdt" % "org.eclipse.jdt.core" % "3.31.0"

// jgit
// https://mvnrepository.com/artifact/org.eclipse.jgit/org.eclipse.jgit
libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "6.3.0.202209071007-r"

// log
libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.19.0"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.19.0"
// https://mvnrepository.com/artifact/org.slf4j/slf4j-simple
libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.3"

// for pipeline operator
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.6"
