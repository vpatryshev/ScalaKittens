package scalakittens.web

import java.net.{ServerSocket, Socket}
import java.util.Date

import scala.io.Source
import scala.language.postfixOps
import scalakittens.IO._
import scalakittens.Result._
import scalakittens.{Good, Result}

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
  def logged = myLog.toString
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
          println(s"→$resp")
          (s1, respond(resp))
      }

      action
    }
  }

  val actOnInput: PartialFunction[String, Action] = sitemap andThen act

  def dispatch(k: String): Action = {
    actOnInput applyOrElse (k, (x:String) => ignore)
  }

  val Format = "([A-Z]+) / ?([^ ?]+)([^ ]*)".r

  def processRequest(serverSocket: ServerSocket)(state: State): State = {
    def process(s: Socket, rq: String): State = {
      println(s"Got request $rq")
      val Format(method, key, params) = rq
      val (newState, resp) = dispatch(key)(state)
      resp(s)
      newState

    }
    val sOpt = Result.forValue(serverSocket.accept())

    val result = for {
      s     ← sOpt
      input ← Result.forValue(Source fromInputStream s.getInputStream)
      lines ← Result.forValue(input.getLines() take 1 toList)
      rq    ← Result(lines.headOption)
      _ = println(s"request=$rq")
      processed = process(s, rq)
    } yield processed

    println(s"Server Result: $result")
    sOpt.foreach(_.close()) andThen result onError log getOrElse state
  }

  lazy val secMgr = new java.lang.SecurityManager()

  /**
    * you may need to do this:
    * sudo nano /Library/Java/JavaVirtualMachines/jdk1.7.0_45.jdk/Contents/Home/jre/lib/security/java.policy
    * and add the following:
    *
    *         permission java.net.SocketPermission "localhost:54777", "accept,connect,listen";
    */
  lazy val canRun: Outcome = Result.forValue(secMgr.checkAccept("localhost", port))

  def runUntilInterrupted() {

    canRun match {
      case Good(_) =>
        val serverSocket = new ServerSocket(port)
        println(s"Server Socket = $serverSocket")
        val result = Stream.iterate(initial)(processRequest(serverSocket)) takeWhile((_:State) => !Thread.interrupted()) foreach(s => ()/*print(s)*/)
        println(s"${new Date()} r=$result")
      case noGood => println(s"Check java security config, it does not allow to run web server: $noGood")
    }
  }

  private lazy val t: Thread = new Thread("Server at " + port) {
    override def run() = runUntilInterrupted()
  }

  def start(): Outcome = this.synchronized {
    myLog.clear()
    try {
      if (!t.isAlive) t.start()
      canRun
    } catch {
      case x:Exception => Result.exception(x)
    }
  }

  def stop() = t.interrupt()
}
