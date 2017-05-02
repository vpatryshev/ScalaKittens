package scalakittens

import java.io.{File, FileOutputStream, OutputStreamWriter}

import org.specs2.mutable.Specification

import scala.io.Source
import scalakittens.OS._

object OS_Test extends Specification {
  val DIGITS = "(\\d+)".r

  "OS" should {

    "provide a convincing pid" in {
      myPid match {
        case DIGITS(pif) ⇒ // ok
        case basura ⇒ failure("Bad pid: " + basura)
      }
      ok
    }

    "provide current directory" in {
      whoami.length > 2 aka ("who are you, " + whoami + "?") must beTrue
      val actual = pwd.toLowerCase
      val expected = (File.separator + whoami).toLowerCase
      actual contains expected aka s"Current dir is $actual, must contain $expected" must beTrue
    }
  }

  "create a temp dir" in {
    val tmp = tmpDir
    tmp.getAbsolutePath startsWith pwd aka ("compare: " + tmp.getAbsolutePath + " and " + pwd) must beTrue
    val file: File = new File(tmp, "testing.txt")
    val out = new OutputStreamWriter(new FileOutputStream(file))
    out.write("ce n'est pas un test")
    out.close()
    Source.fromFile(file).mkString must_== "ce n'est pas un test"
    file.delete()
  }

  "create really really unique temp file" in {
    val tmp = tmpDir
    val file: File = tempFile("format", "ostest")
    val out = new OutputStreamWriter(new FileOutputStream(file))
    out.write("ce n'est pas une programme")
    out.close()
    file.getAbsolutePath endsWith "format.ostest" must beTrue
    file.getAbsolutePath startsWith tmp.getAbsolutePath must beTrue
    file.getAbsolutePath.substring(tmp.getAbsolutePath.length)
    file.delete()
  }

}
