package scalakittens

import org.specs.runner.JUnit4
import org.specs.Specification
import java.io.{PrintWriter, File}
import FS._

class FSTest extends JUnit4(FSTest)

object FSTest extends Specification {
  def tag = "FSTest" + System.currentTimeMillis + System.nanoTime % 1000000

  "File System" should {
    "be able to deal with a folder" in {
      val id = tag
      val name = "tmp/fsIntegrationTest/" + id + "/folder1"
      new File(name).mkdirs()
      val sut = folder(name)
      val parent = sut.parent
      (parent subfolder "folder1" toString) must_== sut.toString
      (folder("tmp") / "fsIntegrationTest" / id / "folder1" toString) must_== name
      (sut file "something.txt" toString) must_== sut.toString + "/something.txt"
    }

    "be able to deal with a text file" in {
      val id = tag
      val foldername = "tmp/fsIntegrationTest/" + id + "/folder1"
      new File(foldername).mkdirs()
      val dir = folder(foldername)
      val grandparent = dir.parent.parent
      val file = dir file "something"
      file.text = "Shri guru devi ommm\n"
      val content: String = file.text
      content must_== "Shri guru devi ommm\n"
      file.text += "Nothing's gonna change my world"
      val newContent: String = file.text
      newContent must_== "Shri guru devi ommm\nNothing's gonna change my world"
      grandparent.file(id + "/folder1/something").text must_== "Shri guru devi ommm\nNothing's gonna change my world"
      file.textOr("never mind") must_==  "Shri guru devi ommm\nNothing's gonna change my world"
      dir file "nonexistent" textOr "what now" must_== "what now"
      val out = new PrintWriter( new File(file.toString) )
      try{ out.print( "this is not a file" ) }
      finally{ out.close }
      val anotherContent: String = file.text
      anotherContent must_== "this is not a file"
    }

    "require that an existing file should actually exist" in {
      val id = tag
      val foldername = "tmp/fsIntegrationTest/" + id + "/folder1"
      new File(foldername).mkdirs()
      val dir = folder(foldername)
      try {
        dir existingFile "Ego non sum"
        fail("this should not be happening")
      } catch {
        case e: Exception => e.getMessage contains foldername mustBe true
      }
      dir file "Ego sum" text = "Ergo cogito"
      (dir existingFile "Ego sum" text) must_== "Ergo cogito"
    }

    "check that a file exists in a folder" in {
      val id = tag
      val foldername = "tmp/fsIntegrationTest/" + id + "/folder1"
      new File(foldername).mkdirs()
      val dir = folder(foldername)
      new File(foldername + "/sub").mkdir()
      val file = dir file "something"
      file.text = "Shri guru devi ommm"
      dir contains "abanamat" mustBe false
      dir contains "something" mustBe true
      dir contains "sub" mustBe true
    }

    "create a subfolder for an absolute path" in {
      val id = tag
      val name = "tmp/fsIntegrationTest/" + id + "/folder1"
      new File(name).mkdirs()
      val sut = folder(name)
      val parent = sut.parent
      (parent subfolder (parent.absolutePath + File.separatorChar + "folder1") toString) must_== sut.toString
    }

    "fail if absolute subfolder is not within the folder" in {
      val id = tag
      val name = "tmp/fsIntegrationTest/" + id + "/folder1"
      new File(name).mkdirs()
      val sut = folder(name)
      val parent = sut.parent
      try {
        parent subfolder (parent.absolutePath + "nonono" + File.separatorChar + "folder1")
        fail("Expected to fail while creating a subfolder with wrong absolute path")
      } catch {
        case iae: IllegalArgumentException => iae.getMessage() contains "fsIntegrationTest" mustBe true
        case x => fail("Expected an iae, got " + x)
      }
    }
  }
}