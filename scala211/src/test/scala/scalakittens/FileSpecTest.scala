package scalakittens
/*
import org.specs.runner.JUnit4
import org.specs.Specification

**
 * Unittests for FileSpec matcher
 *
class FileSpecTest extends JUnit4(FileSpecTest)

object FileSpecTest extends Specification {

  def file(path: String) = new java.io.File(path)

  def checkPositive(cases: (String, String)*) { cases foreach {
      p ⇒ {
        val spec = FileSpec(p._1)
        for (path <- p._2 split ",") {
          spec.accepts(file(path)) aka (path + " in " + p._1) mustBe true
        }
      }
    }
  }

  def checkNegative(cases: (String, String)*) { cases foreach {
      p ⇒ {
        val spec = FileSpec(p._1)
        for (path <- p._2 split ",") {
          spec.accepts(file(path)) aka (path + " in " + p._1) mustBe false
        }
      }
    }
  }

  "FileSpec" should {

    "accept in positive cases" in {
      checkPositive(
        "/myfile" -> "/myfile",
        "/home/vlad/README" -> "/home/vlad/README",
        "/readme.txt" -> "/readme.txt",
        "/J*" -> "/Jack.box,/J1",
        "/a/..." -> "/a/x,/a/x/y/z.t,/a/b/c/d/e/f/g.h",
        "/a/ * /help" -> "/a/a directory /help,/a/b.c/help,/a/b/help",
        "/....c" -> "/a/b/helloworld.c,/strlen.c,/x/y/z/.c",
        "/this is the last one" -> "/this is the last one")
    }

    "deny in negative cases" in {
      checkNegative(
        "/myfile" -> "myfiles,/myfiles,/myfiles,/,/x/myfile,/myfile.xtx",
        "/home/vlad/README" -> "/home/vlad/README.not,/home/README,/HOME/dalv/README",
        "/readme.txt" -> "/readme.txtx,/readme,/readme.txt.bak,/readmenot.txt",
        "/J*" -> "/x/Jack.box,/aJ",
        "/a/..." -> "/,/b/x,/b/a/x/y/z.t,/a.x",
        "/a/ * /help" -> "/a,/a/help,/a/a directory /help.me,/a/b/,/a/b/c/help",
        "/....c" -> "/a/b/helloworld.cpp,/c,/x/y/.c/d",
        "/this is the last one" -> "/this is the last one indeed")
    }
  }
}
*/