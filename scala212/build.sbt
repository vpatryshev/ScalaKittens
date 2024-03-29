organization := "ScalaKittens Inc."

val WhichScala = "2.12.8"

name := s"Scala Kittens Library, Scala $WhichScala"

version := "1.0.0"

scalaVersion := WhichScala
lazy val core = RootProject(file("./core"))
lazy val ml = RootProject(file("./ml"))
lazy val experiments = RootProject(file("./experiments"))

scalacOptions ++= Seq("-feature", "-deprecation", "-encoding", "UTF-8", "-feature", "-target:jvm-1.8", "-unchecked",
    "-Ywarn-adapted-args", "-Ywarn-value-discard", "-Xlint")

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked", "-source", "1.8", "-target", "1.8", "-g:vars")

artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
  artifact.name + "-" + module.revision + "." + artifact.extension
}

resolvers ++= Seq(
  "mvn"            at "https://mvnrepository.com/artifact",
  "snapshots"      at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"       at "http://oss.sonatype.org/content/repositories/releases",
  "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases")

libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"     % "3.2.15" % "test" withSources(),
  "org.scala-lang" % "scala-compiler" % WhichScala,
  "org.scala-lang" % "scala-library" % WhichScala,
  "org.scala-lang" % "scala-reflect" % WhichScala,
  "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "2.2.0",
  "net.liftweb" %% "lift-json" % "3.5.0" withSources(),
  "org.json4s" %% "json4s-native" % "4.0.7" withSources(),
  "org.scalaz" % "scalaz-effect_2.12" % "7.3.7",
  "org.apache.httpcomponents" % "httpclient" % "4.5.14",
  "org.apache.httpcomponents" % "httpmime"   % "4.5.14",
  "joda-time" % "joda-time" % "2.12.5",
  "org.joda" % "joda-convert" % "2.2.2",
  "org.specs2" %% "specs2-core" % "4.19.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.17.0" % "test",
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


