package scalakittens

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import Result.{Errors, NoResult}
import scalakittens.types.Strings.powerString
import net.liftweb.json.JsonAST.{JArray, JField, JObject, JString}


sealed trait Result[+T] extends Container[T] {
  def listErrors: Errors
  def onError[X >: Errors, Y](op: X => Y):Result[T]
  def map[U](f: T => U): Result[U]
  def flatMap[U](f: T => Result[U]): Result[U]
  def returning[U](u: => U): Result[U] = map[U](_ => u)
  def andThen[U](next: => Result[U]): Result[U] = flatMap(_ => next)
  def flatten[U](implicit asResult: T => Result[U]): Result[U] = flatMap(asResult)
  def collect[U](pf: PartialFunction[T, U], onError: T => String): Result[U]
  def asOption: Option[T]
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1]
  def getOrElse[T1 >: T](alt: => T1): T1
  def andAlso[U](other: Result[U]): Result[(T,U)]
  def <*>[U](other: Result[U]): Result[(T,U)] = andAlso(other)
  def foreach(f: T => Unit): Result[T] = {plainForeach(f); this}
  def filter(p: T => Boolean): Result[T]
  def filter(p: T => Boolean, onError: T=> String): Result[T]
  def filter(p: T => Boolean, errorMessage: => String): Result[T] = filter(p, x => errorMessage)
  def filter(flag:Boolean, errorMessage: => String):Result[T] = filter ((x:T) => flag, errorMessage)
  def withFilter(p: T => Boolean): Result[T] = filter(p)
  def filterNot(p: T => Boolean): Result[T] = filter((t:T) => !p(t))
  def filterNot(p: T => Boolean, onError: T=> String): Result[T] =  filter((t:T) => !p(t), onError)
  def filterNot(p: T => Boolean, errorMessage: => String): Result[T] = filterNot(p, x => errorMessage)
  def filterNot(flag:Boolean, errorMessage: => String): Result[T] = filterNot ((x:T) => flag, errorMessage)
  def errorDetails: Option[String]
  def fold[U](good: T => U, bad: Errors => U): U
  def orCommentTheError(message: => Any): Result[T]
  def tap(op: T => Unit): Result[T] // see http://combinators.info/
  def optionally[U](f: T => U => U): U => U = this map f getOrElse identity[U]
  def contains[T1 >: T](x: T1): Boolean
  def iHope: T
}

case class Good[+T](protected val value: T) extends Result[T] with SomethingInside[T] {
  require(value != null, "Cannot accept null in constructor")

  val listErrors: Errors = Nil
  def onError[X >: Errors, Y](op: X => Y):Result[T] = this
  def asOption: Option[T] = Option(value)
  def map[U](f: T=>U): Result[U] = Result.forValue(f(value))
  def flatMap[U](f: T => Result[U]): Result[U] = f(value)
  def collect[U](pf: PartialFunction[T, U], onError: T => String): Result[U] = pf.lift(value) match {
    case Some(t) => Good(t)
    case None    => Result.error(onError(value))
  }
  def fold[U](good: T => U, bad: Errors => U): U = good(value)

  override def toString: String = value match {
    case () => "Good."
    case x => s"Good($x)"
  }

  def orElse[T1 >: T] (next: => Result[T1]): Result[T1] = this
  def getOrElse[T1 >: T](alt: => T1): T1 = value
  def andAlso[U](other: Result[U]): Result[(T, U)] = other.flatMap(u => Good((value, u)))
  protected def plainForeach(f: T => Unit): Unit = f(value)
  def filter(p: T => Boolean): Result[T] = Result.forValue(if (p(value)) this else Empty).flatten
  def filter(p: T => Boolean, onError: T => String): Result[T] =
    Result.forValue(if (p(value)) this else {
      Result.error(onError(value))
    }).flatten
  def errorDetails: Option[String] = None
  def orCommentTheError(message: => Any): Good[T] = this
  def tap(op: T => Unit): Result[T] = {op(value); this}// see http://combinators.info/
  def contains[T1 >: T](x: T1): Boolean = value == x
  def iHope: T = value

  def toJsonString: String = value match {
    case j: CanBeJson => s"""{"Value": ${j.toJsonString}}"""
    case dumb => s"""{"Value": ${dumb.toString.quote}}"""
  }
}

sealed trait NoGood[T] extends NothingInside[T] { self:Result[T] =>
  def filter(p: T => Boolean):Result[T] = self
  def filter(p: T => Boolean, onError: T => String):Result[T] = self
  def getOrElse[T1 >: T](alt: => T1): T1 = alt
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1] = next
  def asOption: Option[T] = None
  def fold[U](good: T => U, bad: Errors => U): U = bad(listErrors)
  def errors: String = listErrors mkString "; "

  def clock: TimeReader = DateAndTime

  val timestamp: Long = clock.currentTime
  def tag: String = s"${ts2b32(timestamp)}"
  override def toString = s"Error: ~$tag($errors)"

  private def base32map = "abdeghjklmnopqrstvxyz.".zipWithIndex.map{case (c,i) => ('a'+i).toChar -> c}.toMap.withDefault(identity)

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
  def contains[T1 >: T](x: T1): Boolean = false
  def iHope: T =
    throw new InstantiationException(errors)
}

sealed trait Bad[T] extends Result[T] with NoGood[T] {
  import Result._

  def onError[X >: Errors, Y](op: X => Y):Result[T] = {op(listErrors); this}
  def map[U](f: T=>U): Result[U] = bad[U](listErrors)
  def flatMap[U](f: T => Result[U]): Result[U] = bad(listErrors)
  def collect[U](pf: PartialFunction[T, U], onError: T => String): Result[U] = bad(listErrors)
  def andAlso[U](other: Result[U]): Result[(T, U)] = bad(listErrors ++ (other.listErrors dropWhile (lastError contains)))
  def lastError: Option[Throwable] = listErrors.lastOption

  private def listMessages(t: Throwable):String =
    Option(t.getCause).fold(Result.messageOf(t))((cause:Throwable) => t.getMessage + ", " + listMessages(cause))

  def errorDetails: Option[String] = Some(listErrors map listMessages mkString "; ")

  private def stackTrace2(t:Throwable): String = {
    val stack = t.getStackTrace
    val tail = stack.dropWhile(el => el.getClassName.contains("Bad") || el.getMethodName == "stackTrace")
    tail.take(35) mkString "\n"
  }

  private def details(t:Throwable): String = {
    val st1 = stackTrace2(t)
    Option(t.getCause).fold(st1)((cause:Throwable) => st1 + "\n\n" + stackTrace2(cause))

    val stack = t.getStackTrace
    val tail = stack.dropWhile(el => el.getClassName.contains("Bad") || el.getMethodName == "stackTrace")
    Result.messageOf(t) + "\n" + (tail.take(35) mkString "\n")
  }

  def stackTrace:String = listErrors map details mkString "\n\n"

  override def toString: String = errors

  def orCommentTheError(message: =>Any): Bad[T] = {
    bad[T](List(recordEvent(message)) ++ listErrors)
  }

  override def equals(other: Any): Boolean = other match {
    case that: Bad[_] => that.listErrors == listErrors
    case basura       => false
  }

  override def hashCode: Int = listErrors.hashCode + tag.hashCode*71

  def tap(op: T => Unit): Result[T] = this
}

case object Empty extends NoResult with NoGood[Nothing] {
  val listErrors: Errors = Nil
  def onError[X >: Errors, Y](op: X => Y):NoResult = this
  def map[U](f: Nothing => U): Result[U] = Empty
  def flatMap[U](f: Nothing => Result[U]): Result[U] = Empty
  def collect[U](pf: PartialFunction[Nothing, U], onError: Nothing => String): Result[U] = Empty

  def andAlso[U](other: Result[U]): Result[(Nothing, U)] = Empty
  def errorDetails: Option[String] = Some("No results")
  def orCommentTheError(message: =>Any): Result[Nothing] = Result.error(message)
  def tap(op: Nothing => Unit): Result[Nothing] = this
}

private object NoException extends Exception {
  def root(x: Throwable): Throwable = x match {
    case NoException => null
    case other => other
  }
}

class ResultException(message:String, source: Throwable = NoException) extends Exception(message, NoException root source) {
  override def toString: String = message
  override def equals(other: Any): Boolean = other match {
    case that: ResultException => that.toString == toString
    case basura                => false
  }
}

// TODO(vlad): stringify better
case class BadResultException(errors: Errors) extends Exception {
  override def getMessage: String = "errors: " + (errors mkString "; ")
}

object Result {

  private def base32map = "abdeghjklmnopqrstvxyz.".zipWithIndex.map { case (c, i) => ('a' + i).toChar -> c }.toMap.withDefault(identity)

  class UnacceptableResult {
    throw new UnsupportedOperationException("This is not a good result, and we can never produce it")
  }

  def fiveCharsIn32(n: Long): String = {
    val s0 = java.lang.Long.toString(n, 32)
    val s1 = s0 map base32map
    "a"*(5-s1.length) + s1
  }

  type Errors = Iterable[Throwable]
  type NoResult = Result[Nothing]
  type Outcome = Result[Unit]

  protected[scalakittens] def forTesting[T](es: Errors, testClock: TimeReader): Result[T] = new Bad[T]() {
    override def clock: TimeReader = testClock
    override def listErrors: Errors = es

    override protected def plainForeach(f: T => Unit): Unit = {}
  }

  def messageOf(t: Throwable): String = Result.forValue(t.getMessage) getOrElse t.toString

  def recordEvent(message:Any):Throwable = {
    new ResultException(""+message)
  }

  implicit def asOutcome(r:Result[_]): Outcome = r andThen OK

  implicit def asBoolean(r: Result[_]): Boolean = r.isGood
  def attempt[T](
    eval: => Result[T],
    onException: Exception => Result[T] = (e: Exception) => exception(e)): Result[T] =
    try { eval } catch { case e: Exception => onException(e) }

  def attempt[T](eval: => Result[T], errMsg: => String): Result[T] =
    attempt(eval, (e: Exception) => exception(e, errMsg) )

  private def legalize[T](optT: => Option[T]): Result[T] = try {
    val o = optT filter (null !=)
    o match {
      case Some(x) => Good(x)
      case _       => Empty
    }
  } catch {
    case e: Exception => exception[T](e)
  }

  //  private def good[T](t: T) = Good(t) // assumes t is not null
  //  private def legal[T](optT: Option[T]) = optT filter (null!=)

  def apply[T](outcome: Outcome): UnacceptableResult = new UnacceptableResult
  implicit def apply[T](optT: Option[T]):                         Result[T] = legalize(optT)
  implicit def apply[T](optT: Option[T], onError: => String):     Result[T] = legalize(optT) orCommentTheError onError
  implicit def apply[T](optT: Option[T], optErr: Option[String]): Result[T] = legalize(optT) match {
    case good: Good[T] => good
    case noGood => optErr match {
      case Some(err) => noGood orCommentTheError err
      case _         => noGood
    }
  }

  def apply[T](tryT: Try[T]): Result[T] = tryT match {
    case Success(t) => Good(t)
    case Failure(x) => exception(x)
  }

  def apply[T](futureT: Future[T]): Result[T] = futureT.value match {
    case None => Empty
    case Some(tryT) => tryT match {
      case Failure(t:Throwable) => exception(t)
      case Success(x) => Good(x)
    }
  }

  def optionOf[T](x: Any): Option[T] = x match {
    case null => None
    case Some(thing) => Result.forValue(thing.asInstanceOf[T]).asOption
    case None        => None
    case notAnOption => Result.forValue(notAnOption.asInstanceOf[T]).asOption
  }

  private[scalakittens] def goodOrBad[T](good: T, bad: String):Result[T] = apply(optionOf(good), Option(bad))

  def goodOrBad[T](data: Iterable[T]): Result[T] = data.toList match {
    case good::null::Nil => forValue(good)
    case good::bad::Nil => goodOrBad(good, bad.toString)
    case wrong::Nil     => error(wrong)
    case x              => error(s"Wrong iterable $x, need one or two elements")
  }

  def apply[T](maybeT: Either[String, T]): Result[T] = maybeT match {
    case Left(bad) => error[T](bad)
    case Right(good) => Good(good)
  }

  def app[X,Y](fOpt:Result[X=>Y])(xOpt:Result[X]):Result[Y] = fOpt<*>xOpt map {case (f,x) => f(x)}

  implicit class Applicator[X,Y](fOpt: Result[X=>Y]) {
    def apply(xOpt:Result[X]): Result[Y] = app(fOpt)(xOpt)
    def apply(x:X): Result[Y] = app(fOpt)(Good(x))
  }

  // TODO(vlad): a) don't accept Results; b) don't accept booleans
  def forValue[T](value: => T):              Result[T] = attempt(apply(Option(value)))
  def forValue[T](value: => T, onError: => String): Result[T] = {
    attempt(apply(Option(value), onError), onError)
  }

  def bad[T](ers: Errors): Bad[T] = new Bad[T] {
    def listErrors: Errors = ers
  }

  def error[T](message: => Any): Bad[T] = bad[T](recordEvent(message)::Nil)

  def exception[T](x: Throwable): Bad[T] = bad(x::Nil)
  def exception[T](x: Throwable, comment: Any): Bad[T] =
    exception[T](x) orCommentTheError comment

  def partition[T](results: IterableOnce[Result[T]]): (List[T], List[Errors]) = {
    val (goodOnes, badOnes) = results.iterator.foldLeft((List.empty[T], List.empty[Errors]))(
      (collected, current) =>
        current match {
          case Good(good)    => (good::collected._1, collected._2)
          case noGood:NoGood[T] => (collected._1, noGood.listErrors.toList::collected._2)
        }
      )
    (goodOnes, badOnes)
  }

  private def badOrEmpty[T](errors: Errors): Result[T] = if (errors.isEmpty) Empty else bad(errors.toSet)

  private def bad[T](results: Result[_]*): Result[T] = bad(results flatMap (_.listErrors))

  implicit class StreamOfResults[T](val source: LazyList[Result[T]]) extends AnyVal {
    def map[U](f: T => U): LazyList[Result[U]] = source map (_ map f)
    def |>[U](op: T => Result[U]): LazyList[Result[U]] = source map (t => t flatMap op)
    def filter(p: T => Result[_]): LazyList[Result[T]] = |> (x => p(x) returning x)
    def toList: List[Result[T]] = source.toList
  }

  def traverse[T](results: IterableOnce[Result[T]]): Result[Iterable[T]] = {
    val (goodOnes, badOnes) = partition(results)

    if (badOnes.nonEmpty) badOrEmpty(badOnes.flatten)
    else Good(goodOnes.reverse)
  }

  def fold(results:Iterable[Outcome]):Outcome = results.iterator.foldLeft(OK:Outcome)(_<*>_)

  def zip[X1,X2](r1: Result[X1], r2: Result[X2]): Result[(X1, X2)] = r1 andAlso r2

  def zip[X1,X2,X3](r1: Result[X1], r2: Result[X2], r3: Result[X3]): Result[(X1,X2,X3)] = {
    (r1,r2,r3) match {
      case (Good(x1),Good(x2),Good(x3)) => Good((x1,x2,x3))
      case (b1,b2,b3)                   => bad(b1, b2, b3)
    }
  }

  def zip[X1,X2,X3,X4](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4]): Result[(X1,X2,X3, X4)] = {
    (r1,r2,r3,r4) match {
      case (Good(x1),Good(x2),Good(x3),Good(x4)) => Good((x1,x2,x3,x4))
      case (b1,b2,b3,b4)                         => bad(b1, b2, b3, b4)
    }
  }

  def zip[X1,X2,X3,X4,X5](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5]): Result[(X1,X2,X3, X4,X5)] = {
    (r1,r2,r3,r4,r5) match {
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5)) => Good((x1,x2,x3,x4,x5))
      case (b1,b2,b3,b4,b5)                               => bad(b1, b2, b3, b4, b5)
    }
  }

  def zip[X1,X2,X3,X4,X5,X6](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6]): Result[(X1,X2,X3, X4,X5,X6)] = {
    (r1,r2,r3,r4,r5,r6) match {
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6)) => Good((x1,x2,x3,x4,x5,x6))
      case (b1,b2,b3,b4,b5,b6)                                     => bad(b1, b2, b3, b4, b5, b6)
    }
  }

  def zip[X1,X2,X3,X4,X5,X6,X7](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6], r7: Result[X7]): Result[(X1,X2,X3, X4,X5,X6,X7)] = {
    (r1,r2,r3,r4,r5,r6,r7) match {
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6),Good(x7)) => Good((x1,x2,x3,x4,x5,x6,x7))
      case (b1,b2,b3,b4,b5,b6,b7)                                     => bad(b1, b2, b3, b4, b5, b6, b7)
    }
  }

  def zip[X1,X2,X3,X4,X5,X6,X7,X8](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6], r7: Result[X7], r8: Result[X8]): Result[(X1,X2,X3, X4,X5,X6,X7,X8)] = {
    (r1,r2,r3,r4,r5,r6,r7,r8) match {
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6),Good(x7),Good(x8)) => Good((x1,x2,x3,x4,x5,x6,x7,x8))
      case (b1,b2,b3,b4,b5,b6,b7,b8)                                     => bad(b1, b2, b3, b4, b5, b6, b7, b8)
    }
  }

  def zip[X1,X2,X3,X4,X5,X6,X7,X8,X9](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6], r7: Result[X7], r8: Result[X8], r9: Result[X9]): Result[(X1,X2,X3, X4,X5,X6,X7,X8,X9)] = {
    (r1,r2,r3,r4,r5,r6,r7,r8,r9) match {
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6),Good(x7),Good(x8),Good(x9)) => Good((x1,x2,x3,x4,x5,x6,x7,x8,x9))
      case (b1,b2,b3,b4,b5,b6,b7,b8,b9)                                                       => bad(b1, b2, b3, b4, b5, b6, b7, b8, b9)
    }
  }

  def zip[X1,X2,X3,X4,X5,X6,X7,X8,X9, X10](r1: Result[X1], r2: Result[X2], r3: Result[X3], r4: Result[X4], r5: Result[X5], r6: Result[X6], r7: Result[X7], r8: Result[X8], r9: Result[X9], r10: Result[X10]): Result[(X1,X2,X3, X4,X5,X6,X7,X8,X9,X10)] = {
    (r1,r2,r3,r4,r5,r6,r7,r8,r9,r10) match {
      case (Good(x1),Good(x2),Good(x3),Good(x4),Good(x5),Good(x6),Good(x7),Good(x8),Good(x9),Good(x10)) => Good((x1,x2,x3,x4,x5,x6,x7,x8,x9, x10))
      case (b1,b2,b3,b4,b5,b6,b7,b8,b9,b10)                                                       => bad(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)
    }
  }

  object OK extends Good(⊤) with Outcome {
    override def toString = "OK"
  }
  def OKif(cond: => Boolean): Outcome = OK filter(_ => cond)
  def OKif(cond: => Boolean, onError: => String): Outcome = OK.filter(_ => cond, _ => onError)
  def OKunless(cond: => Boolean, onError: => String): Outcome = OK.filterNot(_ => cond, _ => onError)
  def BadIf(cond: => Boolean): Outcome = OK filterNot (_ => cond)
  def Oops[T](complaint: Any): Bad[T] = error(complaint)
  def NotImplemented[T]: Bad[T] = Oops[T]("not implemented")
  val ⊤ : Unit = ()

  implicit class RuntimeTyped(x: Any) {
    def typed[T]: Result[T] = Result.forValue(x.asInstanceOf[T])
  }

  def parseJson(source: String): Result[Props] = {
    import net.liftweb._
    val resultJSON = json.parse(source)
    resultJSON match {
      case JObject(List(JField("Value", JObject(keyValueFields)))) =>
        val mp = for {JField(key, JString(value)) <- keyValueFields}
          yield key -> value

        Good(Props(Map(mp: _*)))
      case JObject(List(JField("Errors", JArray(lst)))) =>
        val errs = for {JString(err) <- lst} yield err

        errs match {
          case List() => Empty
          case many => bad(many map recordEvent)
        }
      case _ =>
        error(s"Invalid JSON or Results could not be extracted from JSON ")
    }

  }

  def negate(outcome: Outcome): Result[Unit] with Serializable = outcome match {
    case OK => Empty
    case bad => OK
  }

}

