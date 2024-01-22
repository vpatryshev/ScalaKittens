// If you have JDK 6 and not JDK 7 then replace all three instances of the number 7 to the number 6

organization := "org.scalakittens"

name := "Scala Kittens Library"

version := "1.0.0"

val WhichScala = "2.11.8"

scalaVersion := WhichScala

scalacOptions ++= Seq("-deprecation", "-encoding", "UTF-8", "-feature", "-target:jvm-1.7", "-unchecked",
    "-Ywarn-adapted-args", "-Ywarn-value-discard", "-Xlint")

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked", "-source", "1.8", "-target", "1.8", "-g:vars")

artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
  artifact.name + "-" + module.revision + "." + artifact.extension
}

resolvers ++= Seq(
  "snapshots"                  at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"                   at "http://oss.sonatype.org/content/repositories/releases",
  "scalaz-bintray"             at "http://dl.bintray.com/scalaz/releases",
  "Secured Central Repository" at "https://repo1.maven.org/maven2")

enablePlugins(JmhPlugin)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % WhichScala,
  "org.scala-lang" % "scala-library"  % WhichScala,
  "org.scala-lang" % "scala-reflect"  % WhichScala,
  "org.scalaz" %% "scalaz-core" % "7.1.4" withSources(),
  "org.apache.httpcomponents" % "httpclient" % "4.3.6",
  "org.apache.httpcomponents" % "httpmime"   % "4.3.6",
  "org.specs2" %% "specs2-core" % "3.6" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"//,
//  "org.openjdk.jmh" % "jmh-scala-benchmark-archetype" % "0.5.5" from "http://repo1.maven.org/maven2/org/openjdk/jmh/jmh-scala-benchmark-archetype/0.5.5/jmh-scala-benchmark-archetype-0.5.5.jar"
)

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.10.6" % "test")

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


