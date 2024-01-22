package scalakittens

import java.io.{File, FileOutputStream, OutputStreamWriter}

import scalakittens.IO._
import scalakittens.OS._
import org.specs2.mutable.Specification

class IO_Test extends Specification {

  private def writeFile(file:File, contents: String): Unit = {
    val out = new OutputStreamWriter(new FileOutputStream(file))
    out.write(contents)
    out.close()
  }

  def writeTmpFile(contents:String): File = {
    val file: File = tempFile("format", "exe")
    writeFile(file, contents)
    file
  }

  "IO" >> {
    "check that a good file is good" >> {
      val file = writeTmpFile("ce n'est pas un programme")
      ensureFileIsOk(file) must_== Good(file)
    }

    "check that a small file is bad" >> {
      val file = writeTmpFile(":)")
      ensureFileIsOk(file).isBad must beTrue
    }

    "check that a non-file is bad" >> {
      ensureFileIsOk(tmpDir).isBad must beTrue
    }

    "check that a nonexistent file is bad" >> {
      ensureFileIsOk(new File(tmpDir, "bla bla bla")).isBad must beTrue
    }

    "check that a file with empty name is bad" >> {
      ensureFileIsOk("").isBad must beTrue
    }

    "return good extensionOpt when extension exists" >> {
      val sut = new File("/tmp/test" + System.currentTimeMillis() + "etc.iotest")
      sut.extensionOpt must beSome("iotest")
    }

    "return good extensionOpt when more than one extension exists" >> {
      val sut = new File("/tmp/test" + System.currentTimeMillis() + "etc.com.iotest")
      sut.extensionOpt must beSome("iotest")
    }

    "return None as extensionOpt when more than one extension exists" >> {
      val sut = new File("/tmp/test" + System.currentTimeMillis() + "")
      sut.extensionOpt must beNone
    }

    "provide file extension handling" >> {
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

    "provide file extension handling for the case where there's no exception" >> {
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
