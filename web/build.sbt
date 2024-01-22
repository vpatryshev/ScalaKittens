import sbt._

javaOptions := Seq("-XstartOnFirstThread", "-d32")

name := "Web"

version := "0.1.0"

scalaVersion := "2.12.8"

retrieveManaged := true

libraryDependencies ++= Seq(
  "org.squeryl" %% "squeryl" % "0.9.5-7" withSources(),   // http://squeryl.org/getting-started.html
  "org.apache.pdfbox" % "pdfbox" % "1.4.0", // http://pdfbox.apache.org/download.html
  "org.apache.httpcomponents" % "httpclient" % "4.3.6",
  "org.apache.httpcomponents" % "httpmime"   % "4.3.6",
  "org.scala-lang" % "scala-compiler" % "2.10.4",
  "org.scala-lang" % "scala-library" % "2.10.4",
  "org.specs2" %% "specs2-core" % "3.6" % "test",
  "com.h2database" % "h2" % "1.4.184",
  "org.seleniumhq.selenium" % "selenium-java" % "2.44.0",
  "org.seleniumhq.selenium" % "selenium-firefox-driver" % "2.44.0",
  "com.google.code.findbugs" % "jsr305" % "2.0.2",
  "ch.qos.logback"  %  "logback-classic"   % "1.1.2",
  "com.google.code.gson" % "gson" % "2.3",
  "com.github.nscala-time" %% "nscala-time" % "1.6.0",
  "joda-time" % "joda-time" % "2.7",
  "org.joda" % "joda-convert" % "1.2",
  "com.typesafe.slick" %% "slick" % "2.1.0" withSources(),
  "org.jsoup" % "jsoup" % "1.8.1" withSources(),
  "org.scalaz" %% "scalaz-core" % "7.1.4" withSources()
)

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "http://oss.sonatype.org/content/repositories/releases",
                  "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases")

unmanagedBase := file("/Scala")

parallelExecution := false

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfatal-warnings")

scalacOptions in Test ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfatal-warnings", "-Yrangepos")



// Fat JAR - https://github.com/sbt/sbt-assembly
//assemblySettings

//test in assembly := {} // exclude tests for now - it is failing

//excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
// cp filter {_.data.getName == "swt.jar"}
//}

// TODO(vlad): investigate why it fails
//ideaExcludeFolders += ".idea"

//ideaExcludeFolders += ".idea_modules"


