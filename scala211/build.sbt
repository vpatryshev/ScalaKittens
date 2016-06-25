// If you have JDK 6 and not JDK 7 then replace all three instances of the number 7 to the number 6

organization := "org.scalakittens"

name := "Scala Kittens Library"

version := "0.1.3"

val WhichScala = "2.11.8"

scalaVersion := WhichScala

scalacOptions ++= Seq("-deprecation", "-encoding", "UTF-8", "-feature", "-target:jvm-1.7", "-unchecked",
    "-Ywarn-adapted-args", "-Ywarn-value-discard", "-Xlint")

scalacOptions in (Compile, doc) <++= baseDirectory.map {
  (bd: File) ⇒ Seq[String](
     "-sourcepath", bd.getAbsolutePath,
     "-doc-source-url", "https://github.com/mslinn/changeMe/tree/master€{FILE_PATH}.scala"
  )
}

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked", "-source", "1.8", "-target", "1.8", "-g:vars")

resolvers ++= Seq(
  "Lightbend Releases" at "http://repo.typesafe.com/typesafe/releases"
)

libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"     % "2.2.3" % "test" withSources(),
  "org.scala-lang" % "scala-compiler" % WhichScala,
  "org.scala-lang" % "scala-library" % WhichScala,
  "org.scala-lang" % "scala-reflect" % WhichScala,
  "org.scalaz" %% "scalaz-core" % "7.1.4" withSources(),
  "org.specs2" %% "specs2-core" % "3.6" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
)

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases",
  "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases")

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


