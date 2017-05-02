package ukkonen

import java.io.File

import scala.io.Source

/**
  * Created by vpatryshev on 1/31/17.
  */
object Lib {
  val M = 1<<20

  def int(b: Byte) = b.toInt & 0xff

  val FULL_LOG = false
  def log(what: Any) = if (FULL_LOG) println(System.currentTimeMillis() + " " + what)
  
  def pwd = new File(".").getAbsolutePath

  def fail(msg: => String): Nothing = throw Bad(msg)
  
  def openFile(file: File): Iterator[String] = {
    require(file.canRead, s"Failed to find readable file $file, current dir is $pwd")

    val source = Source.fromFile(file)
    source.getLines()
  }
}

case class Bad(msg: String) extends Exception(msg)