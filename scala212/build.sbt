// If you have JDK 6 and not JDK 7 then replace all three instances of the number 7 to the number 6

organization := "org.scalakittens"

val WhichScala = "2.12.0"

name := s"Scala Kittens Library, Scala $WhichScala"

version := "0.1.4"

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

enablePlugins(JmhPlugin)

libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"     % "3.0.1" % "test" withSources(),
  "org.scala-lang" % "scala-compiler" % WhichScala,
  "org.scala-lang" % "scala-library" % WhichScala,
  "org.scala-lang" % "scala-reflect" % WhichScala,
  "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.0.5",
//  "org.scalaz" % "scalaz-core_2.12.0-RC1" % "7.2.6" withSources(),
  "org.scalaz" % "scalaz-effect_2.12" % "7.3.0-M10",
  "org.apache.httpcomponents" % "httpclient" % "4.3.6",
  "org.apache.httpcomponents" % "httpmime"   % "4.3.6",
  "org.specs2" %% "specs2-core" % "3.8.8" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"//,
//  "org.openjdk.jmh" % "jmh-scala-benchmark-archetype" % "0.5.5" from "http://repo1.maven.org/maven2/org/openjdk/jmh/jmh-scala-benchmark-archetype/0.5.5/jmh-scala-benchmark-archetype-0.5.5.jar"
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


