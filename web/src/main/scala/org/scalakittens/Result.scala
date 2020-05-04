package org.scalakittens

import net.liftweb.json.JsonAST.{JArray, JField, JObject, JString}
import net.liftweb.json._
import org.scalakittens.Result.{Errors, NoResult}

import scala.concurrent.Future
import scala.language.{implicitConversions, postfixOps, reflectiveCalls}
import scala.util.{Failure, Success, Try}

trait Goodness {
  def isGood: Boolean
  def isBad: Boolean = !isGood
}

trait PositiveAttitude extends Goodness { def isGood = true }
trait NegativeAttitude extends Goodness { def isGood = false}


sealed trait Result[+T] extends Goodness with CanBeJson {
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
  def listErrors: Errors
  def onError[X >: Errors, Y](op: X ⇒ Y):Result[T]
  def map[U](f: T ⇒ U): Result[U]
  def returning[U](u: ⇒ U): Result[U] = map[U](_ ⇒ u)
  def flatMap[U](f: T ⇒ Result[U]): Result[U]
  def andThen[U](next: ⇒ Result[U] = Empty):Result[U] = flatMap(_ ⇒ next)
  def flatten[U](implicit asResult: T ⇒ Result[U]): Result[U] = flatMap(asResult)
  def collect[U](pf: PartialFunction[T, U], onError: T ⇒ String): Result[U]
  def toOption: Option[T]
  def orElse[T1 >: T] (next: ⇒ Result[T1]): Result[T1]
  def getOrElse[T1 >: T](alt: ⇒ T1): T1
  def andAlso[U](other: Result[U]): Result[(T,U)]
  protected def foreach_(f: T ⇒ Unit)
  def foreach(f: T ⇒ Unit): Result[T] = {foreach_(f); this}
  def filter(p: T ⇒ Boolean): Result[T]
  def withFilter(p: T ⇒ Boolean): Result[T] = filter(p)
  def filter(p: T ⇒ Boolean, onError: T⇒ String): Result[T]
  def filter(p: T ⇒ Boolean, errorMessage: ⇒ String): Result[T] = filter(p, x ⇒ errorMessage)
  def filter(flag:Boolean, errorMessage: ⇒ String):Result[T] = filter ((x:T) ⇒ flag, errorMessage)
  def filterNot(p: T ⇒ Boolean): Result[T] = filter((t:T) ⇒ !p(t))
  def filterNot(p: T ⇒ Boolean, onError: T⇒ String): Result[T] =  filter((t:T) ⇒ !p(t), onError)
  def filterNot(p: T ⇒ Boolean, errorMessage: ⇒ String): Result[T] = filterNot(p, x ⇒ errorMessage)
  def filterNot(flag:Boolean, errorMessage: ⇒ String):Result[T] = filterNot ((x:T) ⇒ flag, errorMessage)
  def errorDetails: Option[String]
  def fold[U](good: T ⇒ U, bad: Errors ⇒ U):U
  def orCommentTheError(message: ⇒ Any): Result[T]
  def tap(op: T ⇒ Unit): Result[T] // see http://combinators.info/
  def optionally[U](f: T ⇒ U ⇒ U): U ⇒ U = this map f getOrElse identity[U]
}

import Strings._

case class Good[+T](/*private val*/ value: T) extends Result[T] with PositiveAttitude {
  require(value != null, "Cannot accept null in constructor")

  def isEmpty = false
  val listErrors: Errors = Nil
  def onError[X >: Errors, Y](op: X ⇒ Y):Result[T] = this
  def toOption = Some(value)
  def map[U](f: T⇒U): Result[U] = Result.forValue(f(value))
  def flatMap[U](f: T ⇒ Result[U]) = f(value)
  def collect[U](pf: PartialFunction[T, U], onError: T ⇒ String): Result[U] = pf.lift(value) match {
    case Some(t) ⇒ Good(t)
    case None    ⇒ Result.error(onError(value))
  }
  def fold[U](good: T ⇒ U, bad: Errors ⇒ U) = good(value)

  override def toString: String = value match {
    case () ⇒ "Good."
    case x ⇒ s"Good($x)"
  }

  def orElse[T1 >: T] (next: ⇒ Result[T1]): Result[T1] = this
  def getOrElse[T1 >: T](alt: ⇒ T1): T1 = value
  def andAlso[U](other: Result[U]): Result[(T, U)] = other.flatMap(u ⇒ Good((value, u)))
  protected def foreach_(f: T ⇒ Unit) {f(value)}
  def filter(p: T ⇒ Boolean): Result[T] = Result.forValue(if (p(value)) this else Empty).flatten
  def filter(p: T ⇒ Boolean, onError: T ⇒ String): Result[T] = Result.forValue(if (p(value)) this else Result.error(onError(value))).flatten
  def errorDetails: None.type = None
  def orCommentTheError(message: ⇒Any): Result[T] = this
  def tap(op: T ⇒ Unit): Result[T] = {op(value); this}
  def toJsonString: String = value match {
    case j: CanBeJson ⇒ s"""{"Value": ${j.toJsonString}}"""
    case dumb ⇒ s"""{"Value": ${dumb.toString.quote}}"""
  }
}

sealed trait NoGood[+T] extends Result[T] with NegativeAttitude with TimeReader { self ⇒
  def filter(p: T ⇒ Boolean):Result[T] = self
  def filter(p: T ⇒ Boolean, onError: T ⇒ String):Result[T] = self
  protected def foreach_(f: T ⇒ Unit) {}
  def getOrElse[T1 >: T](alt: ⇒ T1): T1 = alt
  def orElse[T1 >: T] (next: ⇒ Result[T1]): Result[T1] = next
  def toOption: Option[T] = None
  def fold[U](good: T ⇒ U, bad: Errors ⇒ U) = bad(listErrors)
  def errors: String = listErrors mkString "; "
  def clock: TimeReader
  val timestamp: Long = clock.currentTime
  def tag: String = s"${ts2b32(timestamp)}"
  override def toString = s"Error: ~$tag($errors)"

  private def base32map = "abdeghjklmnopqrstvxyz.".zipWithIndex.map{case (c,i) ⇒ ('a'+i).toChar -> c}.toMap.withDefault(identity)

  /**
    * Transforms a timestamp to a string
    * First, takes timestamp mod year length, in seconds
    * Then transforms it to base32, skipping probably offending letters (bad words all contain c,f,u letters
    * e.g. 08/28/2015 @ 12:35am (UTC) -> 1440722109 -> "molly"
    *
    * @param n time (millis)
    * @return a string
    */
  private def ts2b32(n: Long): String = {
    val s0 = java.lang.Long.toString(((n+500)/1000) % 31557600, 32)
    val s1 = s0 map base32map
    "0"*(5-s1.length) + s1
  }

}

sealed trait Bad[T] extends Result[T] with NoGood[T] {
  import Result._
  def clock: TimeReader = DateAndTime
  def isEmpty = false

  def onError[X >: Errors, Y](op: X ⇒ Y):Result[T] = {op(listErrors); this}
  def map[U](f: T⇒U): Bad[U] = bad[U](listErrors)
  def flatMap[U](f: T ⇒ Result[U]): Bad[Nothing] = bad(listErrors)
  def collect[U](pf: PartialFunction[T, U], onError: T ⇒ String): Bad[Nothing] = bad(listErrors)
  def lastError: Option[Throwable] = listErrors.lastOption
  def andAlso[U](other: Result[U]): Result[(T, U)] = bad(listErrors ++ (other.listErrors dropWhile (Some(_) == lastError)))

  private def listMessages(t: Throwable):String =
    Option(t.getCause).fold(Result.messageOf(t))((cause:Throwable) ⇒ Result.messageOf(t) + ", " + listMessages(cause))

  def errorDetails = Some(listErrors map listMessages mkString "; ")

  private def stackTrace2(t:Throwable): String = {
    val stack = t.getStackTrace
    val tail = stack.dropWhile(el ⇒ el.getClassName.contains("Bad") || el.getMethodName == "stackTrace")
    tail.take(35) mkString "\n"
  }

  private def details(t:Throwable): String = {
    val st1 = stackTrace2(t)
    Option(t.getCause).fold(st1)((cause:Throwable) ⇒ st1 + "\n\n" + stackTrace2(cause))

    val stack = t.getStackTrace
    val tail = stack.dropWhile(el ⇒ el.getClassName.contains("Bad") || el.getMethodName == "stackTrace")
    Result.messageOf(t) + "\n" + (tail.take(35) mkString "\n")
  }

  def stackTrace:String = listErrors map details mkString "\n\n"

  override def toString: String = errors

  def orCommentTheError(message: ⇒Any): Bad[T] = bad[T](List(recordEvent(message)) ++ listErrors)

  override def equals(other: Any): Boolean = other match {
    case that: Bad[_] ⇒ that.listErrors == listErrors
    case basura       ⇒ false
  }

  override def hashCode: Int = listErrors.hashCode + tag.hashCode*71

  def tap(op: T ⇒ Unit): Result[T] = this
  def toJsonString: String = {
    val msgList = listErrors map (Result.messageOf(_).quote) mkString ", "
    s"""{"Errors": [$msgList]}"""
  }
}

case object Empty extends NoResult with NoGood[Nothing] {
  def clock: TimeReader = DateAndTime
  def isEmpty = true
  val listErrors: Errors = Nil
  def onError[X >: Errors, Y](op: X ⇒ Y):NoResult = this
  def map[U](f: Nothing ⇒ U): Result[U] = Empty
  def flatMap[U](f: Nothing ⇒ Result[U]): Result[U] = Empty
  def collect[U](pf: PartialFunction[Nothing, U], onError: Nothing ⇒ String): Empty.type = Empty

  def andAlso[U](other: Result[U]): Result[(Nothing, U)] = Empty
  def errorDetails = Some("No results")
  def orCommentTheError(message: ⇒Any): Bad[Nothing] = Result.error(message)
  def tap(op: Nothing ⇒ Unit): Result[Nothing] = this
  def toJsonString: String = """{"Errors": []}"""
}

private object NoException extends Exception {
  def root(x: Throwable): Throwable = x match {
    case NoException ⇒ null
    case other ⇒ other
  }
}

class ResultException(message:String, source: Throwable = NoException) extends Exception(message, NoException root source) {
  val check: String = message
  override def toString: String = message
  override def equals(other: Any): Boolean = other match {
    case that: ResultException ⇒ that.toString == toString
    case basura                ⇒ false
  }
}


// TODO(vlad): stringify better
case class BadResultException(errors: Errors) extends Exception {
  override def getMessage: String = "errors: " + (errors mkString "; ")
}

object Result extends ReadsJson {

  class UnacceptableResult {
    throw new UnsupportedOperationException("This is not a good result, and we can never produce it")
  }

  private def base32map = "bdeghijklmnopqrstvxyz".zipWithIndex.map{case (c,i) ⇒ ('a'+i).toChar -> c}.toMap.withDefault(identity)

  def fiveCharsIn32(n: Long): String = {
    val s0 = java.lang.Long.toString(n, 32)
    val s1 = s0 map base32map
    "a"*(5-s1.length) + s1
  }

  type Errors = Traversable[Throwable]
  type NoResult = Result[Nothing]
  type Outcome = Result[Unit]

  protected[scalakittens] def forTesting[T](es: Errors, testClock: TimeReader): Bad[T] = new Bad[T]() {
    override def clock: TimeReader = testClock
    override def listErrors: Errors = es
  }

  def messageOf(t: Throwable): String = Result.forValue(t.getMessage) getOrElse t.toString

  def recordEvent(message:Any):Throwable = {
    new ResultException(""+message)
  }

  implicit def asOutcome(r:Result[_]): Outcome = r andThen OK

  implicit def asBoolean(r: Result[_]): Boolean = r.isGood
  def attempt[T](eval: ⇒ Result[T],
    onException: Exception ⇒ Result[T] = (e:Exception) ⇒ exception(e)): Result[T] =
    try { eval } catch { case e: Exception ⇒ onException(e) }

  def attempt[T](eval: ⇒Result[T], errMsg: ⇒String): Result[T] = attempt(eval, (e:Exception) ⇒ exception(e, errMsg) )

  private def legalize[T](optT: ⇒ Option[T]): Result[T] = try {
    val o = optT filter (null !=)
    o match {
      case Some(x) ⇒ Good(x)
      case _       ⇒ Empty
    }
  } catch {
    case e: Exception ⇒ exception[T](e)
  }

  def apply[T](outcome: Outcome): UnacceptableResult = new UnacceptableResult

  def apply[T](optT: ⇒ Option[T]): Result[T] = legalize(optT)

  def apply[T](optT: ⇒ Option[T], onError: ⇒ String):     Result[T] = legalize(optT) orCommentTheError onError

  def apply[T](optT: ⇒ Option[T], optErr: Option[String]): Result[T] = {
    legalize(optT) match {
      case good: Good[T] ⇒ good
      case noGood ⇒ optErr match {
        case Some(err) ⇒ noGood orCommentTheError err
        case _         ⇒ noGood
      }
    }
  }

  def apply[T](tryT: Try[T]): Result[T] = tryT match {
    case Success(t) ⇒ Good(t)
    case Failure(x) ⇒ exception(x)
  }

  def apply[T](futureT: Future[T]): Result[T] = futureT.value match {
    case None ⇒ Empty
    case Some(tryT) ⇒ tryT match {
      case Failure(t:Throwable) ⇒ exception(t)
      case Success(x) ⇒ Good(x)
    }
  }

  private def optionize[T](x: Any) = x match {
    case null ⇒ None
    case option: Option[T] ⇒ option
    case notAnOption ⇒ Result.forValue(notAnOption.asInstanceOf[T]).toOption
  }

  def goodOrBad[T](good: T, bad: String):Result[T] = apply(optionize(good), Option(bad))

  def goodOrBad[Any](data: Iterable[Any]): Result[Any] = data.toList match {
    case good::bad::Nil ⇒ goodOrBad(good, if(bad == null) null else bad.toString)
    case wrong::Nil     ⇒ error(wrong)
    case x              ⇒ error(s"Wrong iterable $x, need one or two elements")
  }

  def apply[T](maybeT: Either[String, T]): Result[T] = maybeT match {
    case Left(bad) ⇒ error[T](bad)
    case Right(good) ⇒ Good(good)
  }

  def app[X,Y](fOpt:Result[X⇒Y])(xOpt:Result[X]):Result[Y] = fOpt andAlso xOpt map {case (f,x) ⇒ f(x)}

  implicit class Applicator[X,Y](fOpt: Result[X⇒Y]) {
    def apply(xOpt:Result[X]): Result[Y] = app(fOpt)(xOpt)
    def apply(x:X): Result[Y] = app(fOpt)(Good(x))
  }

  def forValue[T](value: ⇒T): Result[T] = attempt(apply(Option(value)))
  def forValue[T](value: ⇒T, onError: ⇒ String): Result[T] = attempt(apply(Option(value), onError), onError)

  def bad[T](ers: Errors): Bad[T] = new Bad[T] {
    def listErrors: Errors = ers
  }

  def error[T](message: ⇒ Any): Bad[T] = bad(recordEvent(message)::Nil)

  def exception[T](x: Throwable): Bad[T] = bad(x::Nil)
  def exception[T](x: Throwable, comment: Any): Bad[T] = {
    exception(x) orCommentTheError comment
  }

  def partition[T](results: TraversableOnce[Result[T]]): (List[T], List[Errors]) = {
    val (goodOnes, badOnes) = ((List.empty[T], List.empty[Errors]) /: results)((collected, current) ⇒
      current match {
        case Good(good)    ⇒ (good::collected._1, collected._2)
        case noGood:NoGood[T] ⇒ (collected._1, noGood.listErrors.toList::collected._2)
      }
    )
    (goodOnes, badOnes)
  }

  private def badOrEmpty[T](errors: Errors): Result[T] = if (errors.isEmpty) Empty else bad(errors.toSet)
  private def bad[T](results: Result[_]*): Result[T] = bad(results flatMap (_.listErrors))

  def traverse[T](results: TraversableOnce[Result[T]]): Result[List[T]] = {
    val (goodOnes, badOnes) = partition(results)

    if (goodOnes.isEmpty || badOnes.nonEmpty) badOrEmpty(badOnes.flatten)
    else Good(goodOnes.reverse)
  }

  implicit class StreamOfResults[T](val source: Stream[Result[T]]) extends AnyVal {
    def map[U](f: T ⇒ U): Stream[Result[U]] = source map (_ map f)
    def |>[U](op: T ⇒ Result[U]): Stream[Result[U]] = source map (t ⇒ t flatMap op)
    def filter(p: T ⇒ Result[_]): Stream[Result[T]] = |> (x ⇒ p(x) returning x)
    def toList: List[Result[T]] = source.toList
  }

  def fold(results:Traversable[Outcome]):Outcome = ((OK:Outcome) /: results)(_ andAlso _) map (_ ⇒ 'OK)

  def zip[X1,X2](r1: Result[X1], r2: Result[X2]): Result[(X1, X2)] = r1 andAlso r2

  def zip[X1,X2,X3](r1: Result[X1], r2: Result[X2], r3: Result[X3]): Result[(X1,X2,X3)] = {
    (r1,r2,r3) match {
      case (Good(x1),Good(x2),Good(x3)) ⇒ Good((x1,x2,x3))
      case (b1,b2,b3)                   ⇒ bad(b1, b2, b3)
    }
  }

  def zip[X1,X2,X3,X4](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4]): Result[(X1,X2,X3, X4)] = {
    (r1,r2,r3,r4) match {
      case (Good(x1),Good(x2),Good(x3),Good(x4)) ⇒ Good((x1,x2,x3,x4))
      case (b1,b2,b3,b4)                         ⇒ bad(b1, b2, b3, b4)
    }
  }

  def zip[X1,X2,X3,X4,X5](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5]): Result[(X1,X2,X3, X4,X5)] = {
    (r1,r2,r3,r4,r5) match {
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5)) ⇒ Good((x1,x2,x3,x4,x5))
      case (b1,b2,b3,b4,b5)                               ⇒ bad(b1, b2, b3, b4, b5)
    }
  }

  def zip[X1,X2,X3,X4,X5,X6](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6]): Result[(X1,X2,X3, X4,X5,X6)] = {
    (r1,r2,r3,r4,r5,r6) match {
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6)) ⇒ Good((x1,x2,x3,x4,x5,x6))
      case (b1,b2,b3,b4,b5,b6)                                     ⇒ bad(b1, b2, b3, b4, b5, b6)
    }
  }

  def zip[X1,X2,X3,X4,X5,X6,X7](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6], r7: Result[X7]): Result[(X1,X2,X3, X4,X5,X6,X7)] = {
    (r1,r2,r3,r4,r5,r6,r7) match {
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6),Good(x7)) ⇒ Good((x1,x2,x3,x4,x5,x6,x7))
      case (b1,b2,b3,b4,b5,b6,b7)                                           ⇒ bad(b1, b2, b3, b4, b5, b6, b7)
    }
  }

  def zip[X1,X2,X3,X4,X5,X6,X7,X8](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6], r7: Result[X7], r8: Result[X8]): Result[(X1,X2,X3, X4,X5,X6,X7,X8)] = {
    (r1,r2,r3,r4,r5,r6,r7,r8) match {
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6),Good(x7),Good(x8)) ⇒ Good((x1,x2,x3,x4,x5,x6,x7,x8))
      case (b1,b2,b3,b4,b5,b6,b7,b8)                                                 ⇒ bad(b1, b2, b3, b4, b5, b6, b7, b8)
    }
  }

  def zip[X1,X2,X3,X4,X5,X6,X7,X8,X9](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6], r7: Result[X7], r8: Result[X8], r9: Result[X9]): Result[(X1,X2,X3, X4,X5,X6,X7,X8,X9)] = {
    (r1,r2,r3,r4,r5,r6,r7,r8,r9) match {
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6),Good(x7),Good(x8),Good(x9)) ⇒ Good((x1,x2,x3,x4,x5,x6,x7,x8,x9))
      case (b1,b2,b3,b4,b5,b6,b7,b8,b9)                                                       ⇒ bad(b1, b2, b3, b4, b5, b6, b7, b8, b9)
    }
  }

  def zip[X1,X2,X3,X4,X5,X6,X7,X8,X9, X10](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6], r7: Result[X7], r8: Result[X8], r9: Result[X9], r10: Result[X10]): Result[(X1,X2,X3, X4,X5,X6,X7,X8,X9,X10)] = {
    (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10) match {
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6),Good(x7),Good(x8),Good(x9),Good(x10)) ⇒ Good((x1,x2,x3,x4,x5,x6,x7,x8,x9, x10))
      case (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)                                                       ⇒ bad(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)
    }
  }

  object OK extends Good(()) with Outcome  {
    override def toString = "OK"
  }
  def OKif(cond: ⇒ Boolean): Result[Unit] = OK filter(_ ⇒ cond)
  def OKif(cond: ⇒ Boolean, onError: ⇒ String): Result[Unit] =
    OK filter (_ ⇒ cond, _ ⇒ onError)
  def OKifNot(cond: ⇒ Boolean): Result[Unit] = OK filterNot (_ ⇒ cond)
  def Oops[T](complaint: Any): Bad[Nothing] = error(complaint)
  def NotImplemented[T]: Bad[Nothing] = Oops[T]("not implemented")

  def parseJson(source: String): Result[Props] = {
    import net.liftweb._
    val resultJSON = json.parse(source)
    resultJSON match {
      case JObject(List(JField("Value",JObject(keyValueFields))))⇒
        val mp = for {JField(key,JString(value))<-keyValueFields}
          yield key->value

        Good(Props(Map(mp:_*)))
      case JObject(List(JField("Errors",JArray(lst)))) ⇒
        val errs = for {JString(err) <- lst} yield err

        errs match {
          case List() ⇒ Empty
          case many ⇒ bad(many map recordEvent)
        }
      case _ ⇒
        error(s"Invalid JSON or Results could not be extracted from JSON ")
    }

  }

  def negate(outcome: Outcome): Result[Unit] = outcome match {
    case OK ⇒ Empty
    case bad ⇒ OK
  }
}

/**
  * Type-safe serializer for [[Result.OK]],[[Bad]] and [[Empty]] objects.
  * Uses ShortTypeHints formats
  *
  * ===Example Usage===
  * {{{
  *    import net.liftweb.json._
  *    import net.liftweb.json.Serialization._
  *    implicit val formats = DefaultFormats.withHints(...) + new ResultJsonSerializer[String]
  *    val claimPropsResult = read[Bad[String]](parse(write(Empty)))
  * }}}
  *
  * @param mf
  *           Implicit manifest parameter
  * @tparam T
  *           The underlying type of the Result object
  *
  */
class ResultJsonSerializer[T](implicit mf:Manifest[T]) extends Serializer[Result[T]]{
  import Result._
  val NoGoodClass: Class[NoGood[T]] = classOf[NoGood[T]]
  val BadClass: Class[Bad[T]] = classOf[Bad[T]]
  val OKOutcomeClass: Class[Good[Symbol]] = classOf[Good[Symbol]]
  override def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Result[T]] = {
    case (TypeInfo(BadClass, _), JObject(List(JField(format.typeHintFieldName, JString("Bad")),JField("listErrors", JArray(lst))))) ⇒
      val errs = for { JString(err) <- lst} yield err
      bad[T](errs map recordEvent)
    case (TypeInfo(NoGoodClass, _), JObject(List(JField(format.typeHintFieldName, JString("Bad")),JField("listErrors", JArray(lst))))) if lst.isEmpty ⇒
      Empty.asInstanceOf[NoGood[T]]
    case (TypeInfo(OKOutcomeClass,_),JObject(List(JField(format.typeHintFieldName,JString("OK")), JField("value",JString("OK"))))) ⇒
      OK.asInstanceOf[Result[T]]
  }

  override def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case a:Bad[_]⇒
      import net.liftweb.json.JsonDSL._
      val ls = for {err<-a.listErrors} yield {
        JString(Result.messageOf(err))
      }
      (format.typeHintFieldName->"Bad")~("listErrors"->JArray(ls.toList))
    case a:NoGood[_] ⇒
      import net.liftweb.json.JsonDSL._
      (format.typeHintFieldName->"Bad")~("listErrors"->JArray(List()))
    case Good('OK) ⇒
      import net.liftweb.json.JsonDSL._
      (format.typeHintFieldName->"OK")~("value"->'OK)
  }
}



