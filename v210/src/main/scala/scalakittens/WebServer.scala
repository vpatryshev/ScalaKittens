package scalakittens

import java.io.IOException
import java.net.{ServerSocket, Socket, SocketException}

import scala.io.Source
import scalakittens.IO._


trait Response {
  def contentType: String
  def dataBytes: Array[Byte]
  def responseCode = "200 OK"
  def headerBytes:Array[Byte] =
    (s"HTTP/1.1 $responseCode\r\n"+
    s"Content-Type:$contentType\r\n"+
    s"Content-Length: ${dataBytes.length}\r\n"+
     "Connection: keep-alive\r\n\r\n").getBytes

  def bytes:Array[Byte] = {
    val bb:Seq[Byte] = headerBytes ++ dataBytes
    bb.toArray
  }
}

abstract class ResourceResponse(path:String, val contentType: String) extends Response {
  override def toString = getClass.getSimpleName+s"($path, ${dataBytes.length} bytes, $responseCode)"
}

case class GifResponse(path:String, override val dataBytes: Array[Byte]) extends ResourceResponse(path, "image/gif")
case class JpgResponse(path:String, override val dataBytes: Array[Byte]) extends ResourceResponse(path, "image/jpeg")
case class PngResponse(path:String, override val dataBytes: Array[Byte]) extends ResourceResponse(path, "image/png")
case class TextResponse(path: String, override val dataBytes: Array[Byte]) extends ResourceResponse(path, "text/html")
case class IconResponse(path: String, override val dataBytes: Array[Byte]) extends ResourceResponse(path, "image/x-icon")

case class LiteralTextResponse(text: String, override val responseCode: String = "200 OK") extends Response {
  def contentType = "text/html"
  lazy val dataBytes = text.getBytes
}

object ServerOps {
  type Responder = Socket => Unit
  def read(path: String): Result[Array[Byte]] = readResourceBytes(if (path == "favicon.ico") path else "/"+path)
  def error404(path: String) = LiteralTextResponse(s"We don't have $path", "404 Not found")
}

/**
 * Test webserver
 */
class WebServer[State](port:Int, initial:State, sitemap: PartialFunction[String, State => (State, Response)]) {
  private val myLog:StringBuilder = new StringBuilder
  private def log(event:Any) = myLog.append("\n" + event)
  def logged = myLog.toString()
  import ServerOps._

  type Action = State => (State, Responder)

  private val respond:Response => Responder =
    (response: Response) =>
    (socket: Socket) => {
    socket.getOutputStream.write(response.bytes)
  }

  private val ignore:Action = s => (s,{println("404"); respond(ServerOps.error404("it..."))})

  val act: ((State) => (State, Response)) => (State) => (State, Responder) = {
    f => {
      val action = (s: State) =>
        f(s) match {
        case (s1, resp) =>
          //println(s"->$resp")
          (s1, respond(resp))
      }

      action
    }
  }

  val actOnInput: PartialFunction[String, Action] = sitemap andThen act

  def dispatch(k: String): Action = {
    actOnInput applyOrElse (k, (x:String) => ignore)
  }

  val Format = "([A-Z]+) /([^ ?]+)([^ ]*) .*".r

  def processRequest(serverSocket: ServerSocket)(state: State): State = {
    try {
      val s = serverSocket.accept

      try {
        val src = Source fromInputStream s.getInputStream
        val stateOption = for (rq <- (src.getLines take 1).toList headOption) yield {
          val Format(method, key, params) = rq
//          print(s"$method($key)")
          val (newState, resp) = dispatch(key)(state)
          resp(s)
          newState
        }
        stateOption getOrElse state

      } catch { case se : SocketException => log(se); state }
      finally { s.close() }
    } catch {
      case ioe: IOException => log(ioe); state
    }
  }

  def runUntilInterrupted() {
    val serverSocket = new ServerSocket(port)
    val result = Stream.iterate(initial)(processRequest(serverSocket)) takeWhile((_:State) => !Thread.interrupted()) foreach(s => ()/*print(s)*/)
    println(s"r=$result")
  }

  private lazy val t: Thread = new Thread("Server at " + port) {
    override def run() = runUntilInterrupted()
  }



  def start() = this.synchronized {
    myLog.clear()
    try {
      println(s"Starting webserver thread $t")
      if (!t.isAlive) t.start()
    } catch {
      case x:Exception => x.printStackTrace()
    }
  }

  def stop() = t.interrupt()
}
