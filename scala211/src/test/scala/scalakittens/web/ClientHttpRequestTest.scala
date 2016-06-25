package scalakittens.web

import java.io._

import org.specs2.mutable.Specification

/**
 * To run this test you kind of need to jump through the loops.
 * I have a jsp that checks if the file sent to it is the right file;
 * this jsp uses my other class, ServerHttpRequest, which I am not sure
 * whether it is worth publishing. Maybe later.
 * So, hmm, trust me, it passes, but this project does not have enough
 * information to prove it.
 */
object ClientHttpRequestTest extends Specification {
  val Url = MockWebServer.root
  val server = MockWebServer.launch

  private val testJsp = "/test3.jsp"
  private val testFileName = "testfile.tmp"
  private val testFileData = "This is the test file"

  private def makeTestFile: File = {
    val testFile: File = new File(testFileName)
    val data: Writer = new FileWriter(testFile)
    data.write(testFileData)
    data.close
    testFile
  }

  "ClientHttpRequest" should {
    "succeed when we set parameters" in {
      val request = new ClientHttpRequest(Url)
      request.addParameter("name", "J.Doe")
      request.addParameter("email", "abuse@spamcop.com")
      request.addParameter("file-upload", makeTestFile)
      val is: InputStream = request.post
      val result = new LineNumberReader(new InputStreamReader(is)).readLine
      result must_== "Success"
    }

    "succeed when we use single post()" in {
      val is: InputStream =  new ClientHttpRequest(Url).post(
                                         "name" -> "J.Doe",
                                         "email" -> "abuse@spamcop.com",
                                         "file-upload" -> makeTestFile)

      val result = new LineNumberReader(new InputStreamReader(is)).readLine
      result must_== "Success"
    }

    "succeed when we pass file via stream" in {
      val request = new ClientHttpRequest(Url)
      request.addParameter("name", "J.Doe")
      request.addParameter("email", "abuse@spamcop.com")
      request.addFile("file-upload", testFileName, new ByteArrayInputStream(testFileData.getBytes))
      val is: InputStream = request.post
      val result = new LineNumberReader(new InputStreamReader(is)).readLine
      result must_== "Success"
    }

//    "Copy a gig like a breeze" in {
//      val in = new FileInputStream("/home/vlad/Downloads/ideaIC-11.1.3.tar.gz")
//      val out = new FileOutputStream("/home/vlad/test.tmp.tar.gz")
//      ClientHttpRequest.pipe(in, out)
//      out.close; in.close
//    }
  }
}
