package scalakittens

import scala.concurrent.Future
import scala.util.{Try, Success, Failure}
import scalakittens.Result.{Errors, NoResult}

sealed trait Result[+T] /* TODO(vlad): extends GenTraversableOnce[T]*/ {
  def isGood: Boolean
  def isBad:  Boolean = !isGood
  def isEmpty: Boolean
  def listErrors: Errors
  def onError[X >: Errors, Y](op: X => Y):Result[T]
  def map[U](f: T => U): Result[U]
  def flatMap[U](f: T => Result[U]): Result[U]
  def andThen[U](next: => Result[U] = Empty):Result[U] = flatMap(_ => next)
  def flatten[U](implicit asResult: T => Result[U]) = flatMap(asResult)
  def collect[U](pf: PartialFunction[T, U], onError: T => String): Result[U]
  def toOption: Option[T]
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1]
  def getOrElse[T1 >: T](alt: => T1): T1
  def <*>[U](other: Result[U]): Result[(T,U)]
  protected def foreach_(f: (T) ⇒ Unit)
  def foreach(f: (T) ⇒ Unit): Result[T] = {foreach_(f); this}
  def filter(p: T => Boolean): Result[T]
  def filter(p: T => Boolean, onError: (T=> String)): Result[T]
  def filter(p: T => Boolean, errorMessage: => String): Result[T] = filter(p, x => errorMessage)
  def filter(flag:Boolean, errorMessage: => String):Result[T] = filter ((x:T) => flag, errorMessage)
  def errorDetails: Option[String]
  def fold[U](good: T => U, bad: Errors => U):U
  def orCommentTheError(message: => Any): Result[T]
  def asOption: Option[T]
}

case class Good[T](value: T) extends Result[T] {
  require(value != null, "Cannot accept null in constructor")
  def isGood = true

  def isEmpty = false
  val listErrors: Errors = Nil
  def onError[X >: Errors, Y](op: X => Y):Result[T] = this
  def toOption = Some(value)
  def map[U](f: T=>U) = Good(f(value))
  def flatMap[U](f: T => Result[U]) = f(value)
  def collect[U](pf: PartialFunction[T, U], onError: T => String) = pf.lift(value) match {
    case Some(t) => Good(t)
    case None    => Result.error(onError(value))
  }
  def fold[U](good: T => U, bad: Errors => U) = good(value)

  override def toString = "Good(" + value.toString + ")"
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1] = this
  def getOrElse[T1 >: T](alt: => T1): T1 = value
  def <*>[U](other: Result[U]): Result[(T, U)] = other.flatMap(u => Good((value, u)))
  protected def foreach_(f: (T) => Unit) {f(value)}
  def filter(p: T => Boolean) = if (p(value)) this else Empty
  def filter(p: T => Boolean, onError: T => String) = if (p(value)) this else Result.error(onError(value))
  def errorDetails = None
  def orCommentTheError(message: =>Any) = this
  def asOption = Some(value)
}

trait NoGood[T] { self:Result[T] =>
  def isGood = false
  def filter(p: T => Boolean):Result[T] = self
  def filter(p: T => Boolean, onError: T => String):Result[T] = self
  protected def foreach_(f: T => Unit) {}
  def getOrElse[T1 >: T](alt: => T1): T1 = alt
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1] = next
  def toOption = None
  def fold[U](good: T => U, bad: Errors => U) = bad(listErrors)
  def asOption = None
  def errors = listErrors mkString "; "
}

class Bad[T](val listErrors: Errors) extends Result[T] with NoGood[T] {
  import Result._

  def isEmpty = false

  def onError[X >: Errors, Y](op: X => Y):Result[T] = {op(listErrors); this}
  def map[U](f: T=>U) = new Bad(listErrors)
  def flatMap[U](f: T => Result[U]) = new Bad(listErrors)
  def collect[U](pf: PartialFunction[T, U], onError: T => String) = new Bad(listErrors)
  def lastError = listErrors.lastOption
  def <*>[U](other: Result[U]): Result[(T, U)] = new Bad(listErrors ++ (other.listErrors dropWhile (Some(_) == lastError)))

  private def listMessages(t: Throwable):String =
    Option(t.getCause).fold(t.getMessage)((cause:Throwable) => t.getMessage + ", " + listMessages(cause))

  def errorDetails = Some(listErrors map listMessages mkString "; ")

  private def stackTrace2(t:Throwable): String = {
    val stack = t.getStackTrace
    val tail = stack.dropWhile(el => el.getClassName.contains("Bad") || el.getMethodName == "stackTrace")
    tail.take(35) mkString "\n"
  }

  private def stackTrace1(t:Throwable): String = {
    val st1 = stackTrace2(t)
    Option(t.getCause).fold(st1)((cause:Throwable) => st1 + "\n\n" + stackTrace2(cause))

    val stack = t.getStackTrace
    val tail = stack.dropWhile(el => el.getClassName.contains("Bad") || el.getMethodName == "stackTrace")
    tail.take(35) mkString "\n"
  }

  def stackTrace:String = listErrors map stackTrace1 mkString "\n\n"

  override def toString = errors

  def orCommentTheError(message: =>Any) = {
    new Bad[T](List(recordEvent(message)) ++ listErrors)
  }

  override def equals(other: Any) = other match {
    case that: Bad[_] => that.listErrors == listErrors
    case basura       => false
  }

  override def hashCode = listErrors.hashCode
}

object Bad {
  def   apply[T](listErrors: Errors): Bad[T] = new Bad[T](listErrors)
  def   apply[T](t: Throwable): Bad[T] = apply[T](t::Nil)
  def unapply(listErrors: Errors): Option[Errors] = Option(listErrors)
}

case object Empty extends NoResult with NoGood[Nothing] {
  def isEmpty = true
  val listErrors: Errors = Nil
  def onError[X >: Errors, Y](op: X => Y):NoResult = this
  def map[U](f: Nothing => U): Result[U] = Empty
  def flatMap[U](f: Nothing => Result[U]): Result[U] = Empty
  def collect[U](pf: PartialFunction[Nothing, U], onError: Nothing => String) = Empty

  def <*>[U](other: Result[U]): Result[(Nothing, U)] = Empty
  def errorDetails = Some("No results")
  def orCommentTheError(message: =>Any) = Result.error(message)
}

class ResultException(message:String) extends Exception(message) {
  override def toString = message
  override def equals(other: Any) = other match {
    case that: ResultException => that.toString == toString
    case basura                => false
  }
}


// TODO(vlad): stringify better
case class BadResultException(errors: Errors) extends Exception {
  override def getMessage: String = "errors: " + (errors mkString "; ")
}

object Result {

  def recordEvent(message:Any):Throwable = new ResultException(""+message)

  implicit def asBoolean(r: Result[_]) = r.isGood
  def attempt[T](eval: => Result[T], onException: (Exception => Result[T]) = (e:Exception) => exception(e)) =
    try { eval } catch { case e: Exception =>
      onException(e)
    }

  def attempt[T](eval: =>Result[T], errMsg: =>String): Result[T] = attempt(eval, (e:Exception) => exception(e, errMsg) )

  private def good[T](t: T) = Good(t) // assumes t is not null
  private def legal[T](optT: Option[T]) = optT filter (null!=)
  def apply[T](optT: Option[T]):                         Result[T] = legal(optT) map good getOrElse Empty
  def apply[T](optT: Option[T], onError: => String):     Result[T] = legal(optT) map good getOrElse error(onError)
  def apply[T](optT: Option[T], optErr: Option[String]): Result[T] = legal(optT) match {
    case Some(x) => Good(x)
    case _ => optErr match {
      case Some(err) => error(err)
      case _         => Empty
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
      case Success(x) => good(x)
    }
  }

  def goodOrBad[T](good: T, bad: String):Result[T] = apply(Option(good), Option(bad))

  def goodOrBad[Any](data: Iterable[Any]): Outcome = data.toList match {
    case good::bad::Nil => goodOrBad(good, if(bad == null) null else bad.toString)
    case wrong::Nil     => error(wrong)
    case x              => error(s"Wrong iterable $x, need one or two elements")
  }

  def apply[T](maybeT: Either[String, T]): Result[T] = maybeT match {
    case Left(bad) => error[T](bad)
    case Right(good) => Good(good)
  }

  def app[X,Y](fOpt:Result[X=>Y])(xOpt:Result[X]):Result[Y] = fOpt<*>xOpt map {case (f,x) => f(x)}

  implicit class Applicator[X,Y](fOpt: Result[X=>Y]) {
    def apply(xOpt:Result[X]) = app(fOpt)(xOpt)
    def apply(x:X) = app(fOpt)(Good(x))
  }

  def forValue[T](value: =>T):              Result[T] = attempt(apply(Option(value)))
  def forValue[T](value: =>T, onError: => String):   Result[T] = attempt(apply(Option(value), onError), onError)

  def error[T](message: => Any): Bad[T] = new Bad[T](recordEvent(message)::Nil)

  def exception[T](x: Throwable): Bad[T] = Bad(x)
  def exception[T](x: Throwable, comment: Any): Bad[T] = exception(x) orCommentTheError comment

  def traverse[T](results: TraversableOnce[Result[T]]): Result[Traversable[T]] = {
    val (goodOnes, badOnes) = ((List.empty[T], List.empty[Errors]) /: results)((collected, current) =>
      current match {
        case Good(good)    => (good::collected._1, collected._2)
        case noGood:NoGood[T] => (collected._1, noGood.listErrors.toList::collected._2)
      }
    )

    val errors = badOnes.flatten
    if (!errors.isEmpty)                  new Bad(errors.toSet) else
    if (goodOnes.isEmpty || !badOnes.isEmpty) Empty             else
                                              Good(goodOnes.reverse)
  }

  type Errors = Traversable[Throwable]
  type NoResult = Result[Nothing]
  type Outcome = Result[Any]

  def fold(results:Traversable[Outcome]):Outcome = ((OK:Outcome) /: results)(_<*>_) map (_ => 'OK)

  object OK extends Good('OK) with Outcome
  def Oops[T](complaint: Any) = error(complaint)
  def NotImplemented[T] = Oops[T]("not implemented")
}

