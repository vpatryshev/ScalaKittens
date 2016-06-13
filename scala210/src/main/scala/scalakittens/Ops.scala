package scalakittens

import language.implicitConversions

import Result._

import collection.JavaConversions
import scala.concurrent.duration._
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.LockSupport

trait Ops { self:AnyRef ⇒
  def scale = 1000

  def askUser(what: String) = {
    println(what+"?")
    what -> readLine
  }

  def sleepSome(dt: Long): Outcome = {
    val actually = (dt + math.random*dt/2+1).toInt * scale / 1000
    //print(s"  Sleeping ${dt*0.001} seconds (actually, $actually ms, scale is $scale)  ")
    Thread sleep actually
    OK
  }

  def sleepSome(dt: Duration): Outcome = {
    sleepSome(dt.toMillis)
  }

  implicit class CanTap[T](x:T) {
    def tap(op:T⇒Unit):T = {op(x); x}
  }

  def tryOr  [T](eval: ⇒T, onError: Exception ⇒ T) =
    try { eval } catch { case e: Exception ⇒
      onError(e)
    }

  def tryOr  [T](eval: ⇒T, orElse: ⇒T) =
    try { eval } catch { case e: Exception ⇒
      orElse
    }

  implicit class Ternary(b: ⇒Boolean) {
    def ?[T](x: ⇒T) = new { def |(y: ⇒T) = if(b) x else y}
  }

  def toSource(x: Any): String = {
    x match {
      case null ⇒ "null"
      case s: String ⇒ "\"" + s.replaceAll("\"", "\\\"") + "\""
      case list:List[_] ⇒ "List(" + list.map(toSource).mkString(", ") + ")"
      case array:Array[_] ⇒ "Array(" + array.map(toSource).mkString(", ") + ")"
      case map:Map[_, _] ⇒ "Map(" + map.map{case(k,v) ⇒ toSource(k) + "->" + toSource(v)}.mkString(", ") + ")"
      case fp@Props(pf) ⇒ pf match {
        case map: Map[_, _] ⇒ "PropTree(" + toSource(map) + ")"
        case _ ⇒ fp.toString()
      }
      case other ⇒ other.toString
    }
  }

  def union[T](sets:TraversableOnce[Set[T]]) = (Set.empty[T]/:sets) (_++_)

  import java.util.{Map ⇒ jum}
  import java.util.{Set ⇒ jus}
  import java.lang.{Iterable ⇒ jli}

  def asScala(x: Any): Any = {
    def iterate[T](i: jli[T]): Iterable[Any] = {
      val in = JavaConversions.iterableAsScalaIterable(i)
      val out = for (y <- in) yield asScala(y)
      out
    }

    x match {
      case null ⇒ Empty
      case r: Result[_] ⇒ r map asScala
      case e: jum.Entry[_,_] ⇒
        val p = asScala(e.getKey) -> asScala(e.getValue)
        p
      case m: jum[_, _] ⇒
        val pairs = iterate(m.entrySet)
        val map = pairs .collect {case(k,v) ⇒ (k,v)} .toMap
        map

      case s: jus[_] ⇒
        val transformed:Iterable[_] = iterate(s)
        transformed.toSet

      case i: jli[_] ⇒
        val transformed:Iterable[_] = iterate(i)
        transformed.toList

      case a: Array[_] ⇒ a.toList.map(asScala)
      case a: Iterable[_] ⇒ a.toList.map(asScala)

      case b: java.lang.Boolean ⇒ b.booleanValue
      case i: java.lang.Integer ⇒ i.intValue
      case l: java.lang.Long ⇒ l.longValue
      case other: Any ⇒ other
    }
  }

  /** Returns shortest possible list of lists xss such that
    *   - xss.flatten == xs
    *   - No sublist in xss contains an element matching p in its tail
    * See Martin Odersky's answer in
    * http://stackoverflow.com/questions/7293617/split-up-a-list-at-each-element-satisfying-a-predicate-scala
    */
  def groupPrefix[T](xs: List[T])(p: T ⇒ Boolean): List[List[T]] = xs match {
    case List() ⇒ List()
    case x :: xs1 ⇒
      val (ys, zs) = xs1 span (!p(_))
      (x :: ys) :: groupPrefix(zs)(p)
  }

  /**
   * Grouping a list by a binary relationship, splitting it into segments where neighbors satisfy p
   * @param xs the list to split
   * @param p the predicate (T,T) ⇒ Boolean
   * @tparam T whatever the type of list elements is
   * @return list of groups, List[List[T] ]
   */
  def groupByRelationship[T](p: (T,T) ⇒ Boolean)(xs: Traversable[T]) = {
    val (seg,acc) = ((List[T](),List[List[T]]()) /: xs) {
      case ((y::ys, a), x) if p(y,x) ⇒ (x ::y ::ys, a)
      case (   (ys, a), x)           ⇒ (x::Nil, ys.reverse::a)
    }
    (seg.reverse::acc).reverse drop 1
  }

  def groupListBy[T,U](xs: List[T])(f: T ⇒ U): List[List[T]] = groupByRelationship[T]((x,y) ⇒ f(x)==f(y))(xs)

  def thread(op: ⇒ Unit) = new Thread {
    override def run() = op
  }

  def spendNotMoreThan[T](time: Duration, extraTimePercent:Int = 1) = new {
    val millis = time.toMillis
    def on(op: ⇒ Result[T]): Result[T] = {
      import LockSupport._
      var res:Result[T] = Empty
      val finalDeadline = System.currentTimeMillis + millis * (100 + extraTimePercent) / 100 + 1
      val done = new AtomicBoolean(false)
      var thrown: Option[Exception] = None
      val worker = new Thread {
        override def run() {
          res = Result.attempt ({
            val r = op
            done.set(true)
            r
          }, (_: Exception) match {
              case ie: InterruptedException ⇒ Empty
              case x: Exception ⇒
                Result.exception(x)
          })
        }
      }
      worker.setName(s"Main.TimeoutAfter $time")
      worker.setPriority(1)
      worker.start()
      worker.join(millis)
      if (worker.isAlive) {
//        Logging.info("Timeout on operation, interrupting...")
        worker.interrupt()
        Thread.`yield`()
        parkUntil(finalDeadline)
      }
//      if (worker.isAlive) {
//        Logging.anError(s"Operation hung, called by $caller")
//      }
      if (done.get) res else {
        Result.error(s"Timeout after $time")
      }
    }
  }

  def stringify(el: StackTraceElement) = el.getFileName + "[" + el.getLineNumber + "]"

  def callerStackTrace = {
    val stack = new Exception("STACK TRACE").getStackTrace
    val tail = stack.dropWhile(el ⇒ (el.getMethodName contains "caller") || (el.getMethodName contains "REPL"))
    tail.tail
  }

  def callers: String = callerStackTrace take 3 map stringify mkString "; "

  def caller: String = {
    val el = callerStackTrace.head
    stringify(el)
  }
}

object Ops extends Ops
