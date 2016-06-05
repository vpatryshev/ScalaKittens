package scalakittens

import scala.language.reflectiveCalls
import org.specs2.mutable.Specification
import IO._
import OS._
import java.io.{OutputStreamWriter, FileOutputStream, File}

class IO_Test extends Specification {
  val DIGITS = "(\\d+)".r

  def writeFile(file:File, contents: String) {
    val out = new OutputStreamWriter(new FileOutputStream(file))
    out.write(contents)
    out.close()
  }

  def writeTmpFile(contents:String): File = {
    val tmp = tmpDir
    val file: File = tempFile("format", "exe")
    writeFile(file, contents)
    file
  }

  "IO" should {
    "check that a good file is good" in {
      val file = writeTmpFile("ce n'est pas une programme")
      ensureFileIsOk(file) must_== Good(file)
    }

    "check that a small file is bad" in {
      val file = writeTmpFile(":)")
      ensureFileIsOk(file).isBad must beTrue
    }

    "check that a non-file is bad" in {
      ensureFileIsOk(tmpDir).isBad must beTrue
    }

    "check that a nonexistent file is bad" in {
      ensureFileIsOk(new File(tmpDir, "bla bla bla")).isBad must beTrue
    }

    "check that a file with empty name is bad" in {
      ensureFileIsOk("").isBad must beTrue
    }

    "return good extensionOpt when extension exists" in {
      val sut = new File("/tmp/test" + System.currentTimeMillis() + "etc.iotest")
      sut.extensionOpt must_== Some("iotest")
    }

    "return good extensionOpt when more than one extension exists" in {
      val sut = new File("/tmp/test" + System.currentTimeMillis() + "etc.com.iotest")
      sut.extensionOpt must_== Some("iotest")
    }

    "return None as extensionOpt when more than one extension exists" in {
      val sut = new File("/tmp/test" + System.currentTimeMillis() + "")
      sut.extensionOpt must_== None
    }

    "provide file extension handling" in {
      val sut = new File("/tmp/test" + System.currentTimeMillis() + "etc.iotest")
      "this is a test" #> sut
      sut.canRead must beTrue
      sut.extension must_== "iotest"
      val renamedOpt = sut.extension = "elf"
      renamedOpt match {
        case Good(renamed) =>
          renamed.f.canRead must beTrue
          renamed.f.getName.endsWith("etc.elf") aka s"Actually have $renamed" must beTrue
          sut.exists() must beFalse
          using (renamed.f) (in => { "this is a test" forall (in.read == _) }) must_== Good(true)
        case bad => failure(s"oops, $bad")
      }
      ok
    }

    "provide file extension handling for the case where there's no exception" in {
      val sut = new File("/tmp/test" + System.currentTimeMillis() + "etc")
      "this is a test" #> sut
      sut.canRead must beTrue
      sut.extension must_== ""
      val renamedOpt = sut.extension = "elf"
      renamedOpt match {
        case Good(renamed) =>
          renamed.f.canRead must beTrue
          renamed.f.getName.endsWith("etc.elf") aka s"Actually have $renamed" must beTrue
          sut.exists() must beFalse
          using (renamed.f) (in => { "this is a test" forall (in.read == _) }) must_== Good(true)
        case bad => failure(s"oops, $bad")
      }
      ok
    }

  }
}
