package scalakittens

sealed trait Result[+T] {
  def isGood: Boolean
  def isBad:  Boolean = !isGood
  val listErrors: Traversable[String]
  def onError(op: Traversable[String] => Unit):Result[T]
  def apply(): T
  def map[U](f: T => U): Result[U]
  def flatMap[U](f: T => Result[U]): Result[U]
  def flatten[U](implicit asResult: T => Result[U]) = flatMap(asResult)
  def collect[U](pf: PartialFunction[T, U], onError: T => String): Result[U]
  def toOption: Option[T]
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1]
  def getOrElse[T1 >: T](alt: => T1): T1
  def <*>[U](other: Result[U]): Result[(T,U)]
  protected def foreach_(f: (T) ⇒ Unit)
  def foreach(f: (T) ⇒ Unit): Result[T] = {foreach_(f); this}
  def filter(p: T => Boolean, onError: T=> String): Result[T]
  def filter(p: T => Boolean, errorMessage: => String): Result[T] = filter(p, x => errorMessage)
  def errorDetails: Option[String]
  def fold[U](good: T => U, bad: Traversable[String] => U) = if (isGood) good(apply()) else bad(listErrors)
  def orCommentTheError(message: => String): Result[T]
}

final case class Good[T](value: T) extends Result[T] {
  require(value != null)
  override def isGood = true
  override val listErrors: Traversable[String] = Nil
  def onError(op: Traversable[String] => Unit): Result[T] = this
  def apply() = value
  def toOption = Some(value)
  def map[U](f: T=>U) = Good(f(value))
  def flatMap[U](f: T => Result[U]) = f(value)
  def collect[U](pf: PartialFunction[T, U], onError: T => String) = pf.lift(value) match {
    case Some(t) => Good(t)
    case None    => Result.error(onError(value))
  }

  override def toString = "Good(" + value.toString + ")"
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1] = this
  def getOrElse[T1 >: T](alt: => T1): T1 = value
  def <*>[U](other: Result[U]): Result[(T, U)] = other.flatMap(u => Good((value, u)))
  protected def foreach_(f: (T) => Unit) {f(value)}
  def filter(p: T => Boolean, onError: T => String) = if (p(value)) this else Result.error(onError(value))
  def errorDetails = None
  def orCommentTheError(message: =>String) = this
}

trait NoGood[T] { self:Result[T] =>
  def isGood = false
  def filter(p: T => Boolean, onError: T => String):Result[T] = self
  protected def foreach_(f: T => Unit) {}
  def getOrElse[T1 >: T](alt: => T1): T1 = alt
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1] = next
  def toOption = None
}

final case class Bad[T](listErrors: Traversable[String]) extends Result[T] with NoGood[T] {
  def onError(op: Traversable[String] => Unit): Result[T] = {op(listErrors); this}
  def apply() = throw new BadResultException(listErrors)
  def map[U](f: T=>U) = Bad(listErrors)
  def flatMap[U](f: T => Result[U]) = Bad(listErrors)
  def collect[U](pf: PartialFunction[T, U], onError: T => String) = Bad(listErrors)
  def <*>[U](other: Result[U]): Result[(T, U)] = Bad(listErrors ++ other.listErrors)

  def errorDetails = Some(summary)

  private def abbr(x: Any) = {
    val full = "" + x
    if (full.length < 100) full else (full.substring(0, 100) + "...")
  }

  private def summary = listErrors mkString "; "

  override def toString = summary
  def orCommentTheError(message: =>String) = Bad(List(message) ++ listErrors)
}

case object Empty extends Result[Nothing] with NoGood[Nothing] {
  val listErrors: Traversable[String] = Nil
  def onError(op: (Traversable[String]) => Unit) = this
  def apply() = throw new BadResultException("No results available"::Nil)
  def map[U](f: Nothing => U): Result[U] = Empty
  def flatMap[U](f: Nothing => Result[U]): Result[U] = Empty
  def collect[U](pf: PartialFunction[Nothing, U], onError: Nothing => String) = Empty

  def <*>[U](other: Result[U]): Result[(Nothing, U)] = Empty
  def errorDetails = Some("No results")
  def orCommentTheError(message: =>String) = Result.error(message)
}

// TODO(vlad): stringify better
case class BadResultException(errors: Traversable[String]) extends Exception {
  override def getMessage: String = "errors: " + (errors mkString "; ")
}

object Result {
  def attempt[T](eval: =>Result[T]): Result[T] = try { eval } catch { case e: Exception => exception(e) }
  def attempt[T](eval: =>Result[T], errMsg: =>String): Result[T] = try { eval } catch { case e: Exception => exceptionWithMessage(e, errMsg) }
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

  def goodOrBad[T](good: T, bad: String):Result[T] = apply(Option(good), Option(bad))

  def goodOrBad[Any](data: Iterable[Any]): Result[Any] = data.toList match {
    case good::bad::Nil => goodOrBad(good, if(bad == null) null else bad.toString)
    case wrong::Nil     => error(wrong)
    case x              => error("Wrong iterable " + x + ", need one or two elements")
  }

  def apply[T](maybeT: Either[String, T]): Result[T] = maybeT match {
    case Left(bad) => error[T](bad)
    case Right(good) => Good(good)
  }

  def forValue[T](value: =>T):              Result[T] = attempt(apply(Option(value)))
  def forValue[T](value: =>T, onError: => String):   Result[T] = attempt(apply(Option(value), onError), onError)

  def error[T](message: => Any): Bad[T] = Bad(("" + message)::Nil)

  def exception[T](x: Exception): Bad[T] = error(x)
  def exception[T]           (x: Exception, suspect: => Any): Bad[T] = error("" + x + " on " + suspect)
  def exceptionWithMessage[T](x: Exception, message: String): Bad[T] = error("" + message + ": " + x)

  def traverse[T](results: Traversable[Result[T]]): Result[Traversable[T]] = {
    if (results.isEmpty) Empty else {
      val goodValues = results collect {case Good(x) => x}
      val badOnes  = results.filter(r => r.isBad && r != Empty)
      if (badOnes.isEmpty) Good(goodValues)
      else {
        if (badOnes.isEmpty) Empty
        else                 Bad(badOnes.toList.flatMap(_.listErrors))
      }
    }
  }
}