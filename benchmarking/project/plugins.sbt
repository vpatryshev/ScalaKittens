addSbtPlugin("pl.project13.scala" % "sbt-jmh" % {
  val is = io.Source.fromFile(new File("../version.sbt")).mkString
  val it = is.replaceAll("version in ThisBuild := ", "").replaceAll("\"", "").replaceAll("\n", "")
  it
}, "0.13", "2.10")
