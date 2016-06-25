package scalakittens.web

import scalakittens.{Good, Result}

/**
  * Created by vpatryshev on 12/28/15.
  */
trait StatelessWebServer {
  def port: Int
  type State = Unit
  val initialState = ()
  val TextFmt = ".+\\.(html|js|css)".r
  val GifFmt = ".+\\.(gif)".r
  val PngFmt = ".+\\.(png)".r
  val IcoFmt = ".+\\.(ico)".r
  val JpegFmt = "([^\\.])+".r

  def provideResponseBytes(path: String): Result[Array[Byte]]

  def findResponse(path:String): Response = (path, provideResponseBytes(path)) match {
    case (TextFmt(_),Good(bytes))   => TextResponse(path, bytes)
    case (GifFmt(ext),Good(bytes))  => GifResponse(path, bytes)
    case (JpegFmt(ext), Good(bytes)) => JpgResponse(path, bytes)
    case (PngFmt(ext), Good(bytes))  => PngResponse(path, bytes)
    case (IcoFmt(ext), Good(bytes))   => IconResponse(path, bytes)
    case (_, err) => ServerOps.error404(s"$path: ${err.listErrors mkString "; "}")
  }

  val sitemap: PartialFunction[String, Unit => (Unit, Response)] = findResponse(_) match {
    case r => (_:Unit) => ((), r)
  }

  val server = new WebServer[Unit](port, initialState, sitemap)

  def launch: Result[WebServer[Unit]] = {
    val t = new Thread {
      override def run(): Unit = {
        main(Array())
      }
    }
    t.start()
    server.canRun returning server
  }

  def main(args: Array[String]) {
    println(server.start())
  }
}
