package org.scalakittens.web

import java.io.IOException
import java.net.{ServerSocket, Socket, SocketException}
import java.util.Date

import org.scalakittens.IO._
import org.scalakittens.{Good, Result}
import org.scalakittens.Result.Outcome

import scala.io.Source
import scala.language.postfixOps
import scala.util.matching.Regex

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
  override def toString: String = getClass.getSimpleName+s"($path, ${dataBytes.length} bytes, $responseCode)"
}

case class GifResponse(path:String, override val dataBytes: Array[Byte]) extends ResourceResponse(path, "image/gif")
case class JpgResponse(path:String, override val dataBytes: Array[Byte]) extends ResourceResponse(path, "image/jpeg")
case class PngResponse(path:String, override val dataBytes: Array[Byte]) extends ResourceResponse(path, "image/png")
case class TextResponse(path: String, override val dataBytes: Array[Byte]) extends ResourceResponse(path, "text/html")
case class IconResponse(path: String, override val dataBytes: Array[Byte]) extends ResourceResponse(path, "image/x-icon")

case class LiteralTextResponse(text: String, override val responseCode: String = "200 OK") extends Response {
  def contentType = "text/html"
  lazy val dataBytes: Array[Byte] = text.getBytes
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
  def logged: String = myLog.toString
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
          //println(s"→$resp")
          (s1, respond(resp))
      }

      action
    }
  }

  val actOnInput: PartialFunction[String, Action] = sitemap andThen act

  def dispatch(k: String): Action = {
    actOnInput applyOrElse (k, (x:String) => ignore)
  }

  val Format: Regex = "([A-Z]+) /([^ ?]+)([^ ]*) .*".r

  def processRequest(serverSocket: ServerSocket)(state: State): State = {
    try {
      val s = serverSocket.accept

      try {
        val src = Source fromInputStream s.getInputStream
        val stateOption = for (rq ← (src.getLines take 1).toList headOption) yield {
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
      case ioe: IOException =>
        log(ioe)
        state
      case something: Throwable =>
        something.printStackTrace()
        throw something
    }
  }

  lazy val secMgr = new java.lang.SecurityManager()

  /**
    * you may need to do this:
    * sudo nano /Library/Java/JavaVirtualMachines/jdk1.7.0_45.jdk/Contents/Home/jre/lib/security/java.policy
    * and add the following lines:
    *
    *         permission java.net.SocketPermission "localhost:8096", "accept,connect,listen";
    *         permission java.net.SocketPermission "127.0.0.1:8096", "accept,connect,listen";
    */
  lazy val canRun: Outcome = Result.forValue(secMgr.checkAccept("localhost", port))

  def runUntilInterrupted() {
    println(s"Just starting our test web server on port $port")

    canRun match {
      case Good(_) =>
        val serverSocket = new ServerSocket(port)
        println(s"Server Socket = $serverSocket")
        val result: Unit = Stream.iterate(initial)(processRequest(serverSocket)) takeWhile((_:State) => !Thread.interrupted()) foreach(s => ()/*print(s)*/)
        println(s"${new Date()} r=$result")
      case noGood => println(s"Check java security config, it does not allow to run web server: $noGood")
    }
  }

  private lazy val t: Thread = new Thread("Server at " + port) {
    override def run(): Unit = runUntilInterrupted()
  }



  def start(): Outcome = this.synchronized {
    myLog.clear()
    try {
      println(s"${new Date()} Starting webserver thread $t")
      if (!t.isAlive) t.start()
      canRun
    } catch {
      case x:Exception => Result.exception(x)
    }
  }

  def stop(): Unit = t.interrupt()
}
