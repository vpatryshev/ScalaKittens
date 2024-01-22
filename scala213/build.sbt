
ThisBuild / organization := "ScalaKittens Inc."

val WhichJava = "17"
val WhichScala = "2.13.6"
val name = s"Scala Kittens Library, Scala $WhichScala"

// Global / excludeLintKeys projectName

ThisBuild / version := "1.0.0"

ThisBuild / scalaVersion := WhichScala

scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "--explain-types",
  "-feature",
  "-deprecation",
//  s"-release:$WhichJava",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:existentials",
  "-language:postfixOps",
  "-opt:redundant-casts",
  "-unchecked",
  "-Wconf:cat=lint-multiarg-infix:silent",
  "-Ywarn-value-discard", "-Xlint"
)

javacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-Xlint:deprecation",
  "-Xlint:unchecked",
//  "-source", WhichJava,
//  "-target", WhichJava,
  "-g:source")

artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
  s"${artifact.name}-${module.revision}.${artifact.extension}"
}

resolvers ++= Seq(
  "mvn"            at "https://mvnrepository.com/artifact",
  "snapshots"      at "https://oss.sonatype.org/content/repositories/snapshots",
  "releases"       at "https://oss.sonatype.org/content/repositories/releases",
  "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases")


libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"     % "3.2.15" % "test" withSources(),
  "org.scala-lang" % "scala-compiler" % WhichScala,
  "org.scala-lang" % "scala-library" % WhichScala,
  "org.scala-lang" % "scala-reflect" % WhichScala,
  "org.scala-lang.modules" % "scala-parser-combinators_2.13" % "2.2.0",
  "net.liftweb" %% "lift-json" % "3.5.0" withSources(),
  "org.json4s" %% "json4s-native" % "4.0.7" withSources(),
//  "org.scalaz" % "scalaz-effect_2.13" % "7.3.7",
  "org.apache.httpcomponents" % "httpclient" % "4.5.14",
  "org.apache.httpcomponents" % "httpmime"   % "4.5.14",
  "joda-time" % "joda-time" % "2.12.5",
  "org.joda" % "joda-convert" % "2.2.2",
  "org.scala-lang.modules" % "scala-parallel-collections_2.13" % "1.0.4",
  "org.specs2" %% "specs2-core" % "4.19.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.17.0" % "test",
)

logLevel := Level.Warn

// Level.INFO is needed to see detailed output when running tests
// (shows errors when run with 1.9.2) logLevel in test := Level.Info

// define the statements initially evaluated when entering 'console', 'console-quick', but not 'console-project'
// (same problem) initialCommands in console := """""".stripMargin
