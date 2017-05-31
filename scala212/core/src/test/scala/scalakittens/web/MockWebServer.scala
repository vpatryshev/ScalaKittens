package scalakittens.web

import scalakittens.Result

/**
  * Purpose: serve as a web server, for testing
  */
object MockWebServer extends StatelessWebServer {
  def port = 54777
  lazy val root = Url(s"http://127.0.0.1:$port")

  def provideResponseBytes(path: String): Result[Array[Byte]] = ServerOps.read(path)
}
