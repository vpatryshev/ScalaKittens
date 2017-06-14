
organization := "org.scalakittens"

val WhichScala = "2.12.2"

name := s"Scala Kittens Machine Learning, Scala $WhichScala"

version := "0.1.0"

scalaVersion := WhichScala

lazy val core = RootProject(file("../core"))

val main = Project(id = "ml", base = file(".")).dependsOn(core)

scalacOptions ++= Seq("-feature", "-deprecation", "-encoding", "UTF-8", "-feature", "-target:jvm-1.8", "-unchecked",
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
  "org.scalatest"  %% "scalatest"     % "3.0.1" % "test" withSources(),
  "org.scala-lang" % "scala-compiler" % WhichScala,
  "org.scala-lang" % "scala-library" % WhichScala,
  "org.scala-lang" % "scala-reflect" % WhichScala,
  "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.0.5",
  "org.scalaz" % "scalaz-effect_2.12" % "7.3.0-M10",
  "org.apache.httpcomponents" % "httpclient" % "4.3.6",
  "org.apache.httpcomponents" % "httpmime"   % "4.3.6",
  "org.specs2" %% "specs2-core" % "3.8.8" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.sameersingh.scalaplot" % "scalaplot" % "0.0.4"
)

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases",
  "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases")

unmanagedJars in Compile := {
  val base = file("lib/12")
  val jars = Seq(base / "scalaplot.jar")
  jars.classpath
}

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

