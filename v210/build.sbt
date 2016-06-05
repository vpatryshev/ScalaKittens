// If you have JDK 6 and not JDK 7 then replace all three instances of the number 7 to the number 6

organization := "org.scalakittens"

name := "Scala Kittens Library"

version := "0.1.3"

scalaVersion := "2.10.6"

scalacOptions ++= Seq("-deprecation", "-encoding", "UTF-8", "-feature", "-target:jvm-1.7", "-unchecked",
    "-Ywarn-adapted-args", "-Ywarn-value-discard", "-Xlint")

scalacOptions in (Compile, doc) <++= baseDirectory.map {
  (bd: File) => Seq[String](
     "-sourcepath", bd.getAbsolutePath,
     "-doc-source-url", "https://github.com/mslinn/changeMe/tree/master€{FILE_PATH}.scala"
  )
}

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked", "-source", "1.8", "-target", "1.8", "-g:vars")

resolvers ++= Seq(
  "Lightbend Releases" at "http://repo.typesafe.com/typesafe/releases"
)

//"org.specs2" %% "specs2-core" % "3.6" % "test",
//val specs2 = "org.specs2" %% "specs2" % "2.3.11" % "test"

libraryDependencies ++= Seq(
  "org.scalatest"           %% "scalatest"     % "2.2.3" % "test" withSources(),
//  "org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4",
  "org.specs2" %% "specs2-core" % "3.6" % "test"
)

logLevel := Level.Warn

// Only show warnings and errors on the screen for compilations.
// This applies to both test:compile and compile and is Info by default
logLevel in compile := Level.Warn

// Level.INFO is needed to see detailed output when running tests
logLevel in test := Level.Info

// define the statements initially evaluated when entering 'console', 'console-quick', but not 'console-project'
initialCommands in console := """
                     |""".stripMargin

cancelable := true


