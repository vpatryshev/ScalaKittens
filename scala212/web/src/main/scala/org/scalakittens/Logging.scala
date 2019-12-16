package org.scalakittens

import java.io.{PrintStream, PrintWriter}

import org.scalakittens.Result.{Errors, OK, Outcome}
import org.scalakittens.types.ResultType

import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}

// All logging goes here; this is just the first step
// See https://github.com/HealthExpense/Backend2/issues/150

trait Logging extends TimeReader {
  private val queue: mutable.Queue[String] = new mutable.Queue[String]()
  private val QueueSize = 100
  def dumpLatest: String = queue mkString "\n"

  protected def <<(message: String): Unit = {
    import Logging.count
    count += 1
    val line = s"$jodaNow($count) $message"
    queue enqueue line
    if (queue.length > QueueSize) queue.dequeue()
    Logging.printer.println(line)
  }

  // TODO(vlad): introduce levels
  // TODO(vlad): don't need formatting; lazy format is enough
  def log(tag: String, format: => Any, params: Any*): Unit = {
    val message =
    try {
      val formatted = if (params.isEmpty) format else format.toString.format(params: _*)
      Some(s"[$tag]: $formatted")
    } catch {
      case t: Throwable => try {
        dumpStackTrace(t)
        <<(s"[$tag]: !!!!!Failed to format <<$format>>: ${params mkString ","} ((${t.getMessage}))")
        None
      } catch {
        case t: Throwable => try {
          <<(s"[$tag]: !!!!!Failed to stringify parameters in <<$format>> (${t.getMessage})")
          None
        }
      }
    }

    try {
      message foreach <<
    } catch {
      case e: Exception => System.err.println(s"Log failed: $message $e"); e.printStackTrace()
    }
  }

  def logger(tag: String): PrintWriter = new PrintWriter(System.out)

  var lastError: Any = ""

  def anError(msg: => Any): Bad[Nothing] = {
    log("Error", msg)
    Result.error[Nothing](msg)
  }

  def dumpStackTrace(xOrNull: Throwable): Unit = {
    Some(xOrNull) foreach { x =>
      val stack = x.getStackTrace
      val tail = stack.dropWhile(el => el.getClassName.contains("Logging") || el.getMethodName == "error")
      tail.take(35).foreach(println)
    }
  }

  def dumpStackTrace(): Unit = dumpStackTrace(new Exception("STACK TRACE"))

  def ifDebug(op: => Unit): Unit = if (Logging.isDebug || Logging.isTest) op

  def error(msg: => Any): Bad[Nothing] = {
    if (msg != lastError) {
      lastError = msg
      if (Logging.isDebug) {
        println("---in debug mode---")
        dumpStackTrace()
      }
      anError(msg)
    }
    Result.error[Nothing](msg)
  }

  // Displays error message and returns FALSE
  def errorResult(msg: => Any): Boolean = {
    error(msg)
    false
  }

  def warn        (message:  =>Any): Result.OK.type = { log("Warn",    message); OK }
  def warnIf      (cond: => Boolean)(message: =>Any): Any = if (cond) warn(message)
  def info        (message:  =>String):Unit = log("Info",    message)
  def info[T](x:T, message: =>String):T     = {info(message); x}
  def debug       (message:  =>String): Unit = {
    if (Logging.isDebug)    log("Debug", message)
  }
  def bigdebug    (message: =>String): Unit = if (Logging.isBigDebug) log("DEBUG", message)
  def dump[T](x: =>T): T = {log("dump", "" + x); x}
  def todo(text: =>String): Bad[Null] = {
    val msg = "TODO: implement this." + text
    <<(msg)
    Result.error(msg)
  }
  def exception(ex: Throwable, msg: =>String): Outcome = {
      val stackTrace = ex.getStackTrace take 60 mkString "\n    "
      anError(s"$msg\n$ex\nStacktrace:\n    $stackTrace")
  }


  def report[T](message: String, result: Result[T]): Result[T] = {
    result match {
      case Good(stuff) => info(message format stuff)
      case bad: Bad[_] =>
        anError(bad.errors)
// no need actually        if (Logging.isDebug) println(bad.stackTrace)

      case _       => anError(s"$message $result - no results found.")
    }
    result
  }
}

object Logging extends Logging {
  var count: Int = 0
  var printer: PrintStream = System.out

  sealed trait Mode {
    def isDebug: Boolean = isBigDebug
    def isTest: Boolean = false
    def isProduction: Boolean = !isTest && !isDebug
    def isBigDebug: Boolean = false
    override def toString: String = this.getClass.getName.split("\\$")(1)
  }

  object DebugMode extends Mode {
    override def isDebug = true
  }

  object ProductionMode extends Mode {
    override def isDebug = false
  }

  object TestMode extends Mode {
    override def isTest: Boolean = true

    override def isDebug: Boolean = false
  }

  object BigDebugMode extends Mode {
    override def isTest: Boolean = false

    override def isBigDebug: Boolean = true
  }

  var mode: Mode = ProductionMode

  def setDebugMode(on: Boolean) {
    if (on) mode = DebugMode }

  def isProduction: Boolean = mode.isProduction
  def isDebug: Boolean = mode.isDebug
  def isTest: Boolean = mode.isTest
  def isBigDebug: Boolean = mode.isBigDebug

  var prefix:() => String = () => ""

  private def logWithContext(kind: String, sc: StringContext, args: Any*): Unit = {
    val strings = sc.parts.iterator
    val expressions = args.iterator
    var buf = new StringBuffer(strings.next())
    while (strings.hasNext) {
      buf append expressions.next
      buf append strings.next
    }
    log(kind, buf.toString.trim)
  }

  implicit class FatalLogger(val sc: StringContext) extends AnyVal {
    def fatal(args: Any*): ResultType.Value = {
      logWithContext("Fatal", sc, args: _*)
      ResultType.Failure
    }
  }

  implicit class FailureLogger(val sc: StringContext) extends AnyVal {
    def failure(args: Any*): ResultType.Value = {
      logWithContext("Fatal", sc, args: _*)
      ResultType.Failure
    }
  }

  implicit class ErrorReporter[T](res: Result[T]) {
    def reportingError: Result[T] = res onError ((ee: Errors) => error(ee))
  }

}
