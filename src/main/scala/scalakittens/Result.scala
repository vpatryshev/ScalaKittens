package scalakittens


sealed trait Result[+T] {
  def isGood: Boolean
  def isBad:  Boolean = !isGood
  val listErrors: Traversable[String]
  def onError(op: Traversable[String] => Unit)
  def apply(): T
  def map[U](f: T => U): Result[U]
  def flatMap[U](f: T => Result[U]): Result[U]
  def flatten[U](implicit asResult: T => Result[U]) = flatMap(asResult)
  def collect[U](pf: PartialFunction[T, U], onError: => String): Result[U]
  def toOption: Option[T]
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1]
  def getOrElse[T1 >: T](alt: => T1): T1
  def <*>[U](other: Result[U]): Result[(T,U)]
  def foreach(f: (T) ⇒ Unit): Unit
  def filter(p: T => Boolean, onError: T=> String): Result[T]
  def filter(p: T => Boolean, errorMessage: => String): Result[T] = filter(p, x => errorMessage)
  def errorDetails: Option[String]
  def fold[U](good: T => U, bad: Traversable[String] => U) = if (isGood) good(apply()) else bad(listErrors)
}

final case class Good[T](value: T) extends Result[T] {
  require(value != null)
  override def isGood = true
  override val listErrors: Traversable[String] = Nil
  def onError(op: Traversable[String] => Unit): Unit = {}
  def apply() = value
  def toOption = Some(value)
  def map[U](f: T=>U) = Good(f(value))
  def flatMap[U](f: T => Result[U]) = f(value)
  def collect[U](pf: PartialFunction[T, U], onError: => String) = Result(pf.lift(value), onError)
  override def toString = "Good(" + value.toString + ")"
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1] = this
  def getOrElse[T1 >: T](alt: => T1): T1 = value
  def <*>[U](other: Result[U]): Result[(T, U)] = other.flatMap(u => Good((value, u)))
  def foreach(f: (T) => Unit) {f(value)}
  def filter(p: T => Boolean, onError: T => String) = if (p(value)) this else Result.error(onError(value))
  def errorDetails = None
}

final case class Bad[T](listErrors: Traversable[String]) extends Result[T] {
  def isGood = false
  def onError(op: Traversable[String] => Unit): Unit = {op(listErrors)}
  def apply() = throw new BadResultException(listErrors)
  def toOption = None
  def map[U](f: T=>U) = Bad(listErrors)
  def flatMap[U](f: T => Result[U]) = Bad(listErrors)
  def collect[U](pf: PartialFunction[T, U], onError: => String) = Bad(listErrors)
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1] = next
  def getOrElse[T1 >: T](alt: => T1): T1 = alt
  def <*>[U](other: Result[U]): Result[(T, U)] = Bad(listErrors ++ other.listErrors)

  def foreach(f: T => Unit) {}
  def filter(p: T => Boolean, onError: T => String) = this
  def errorDetails = Some(summary)

  private def abbr(x: Any) = {
    val full = "" + x
    if (full.length < 100) full else (full.substring(0, 100) + "...")
  }

  private def summary = listErrors mkString "; "

  override def toString = summary
}

final case object NoResult extends Result[Nothing] {
  def isGood: Boolean = false
  val listErrors: Traversable[String] = Nil
  def onError(op: (Traversable[String]) => Unit) {}
  def apply() = throw new BadResultException("No results available"::Nil)
  def map[U](f: Nothing => U): Result[U] = NoResult
  def flatMap[U](f: Nothing => Result[U]): Result[U] = NoResult
  def collect[U](pf: PartialFunction[Nothing, U], onError: => String) = NoResult
  def toOption: Option[Nothing] = None

  def getOrElse[T](alt: => T): T = alt
  def orElse[T1 >: Nothing] (next: => Result[T1]): Result[T1] = next

  def <*>[U](other: Result[U]): Result[(Nothing, U)] = NoResult
  def foreach(f: (Nothing) => Unit) {}
  def filter(p: Nothing => Boolean, onError: Nothing => String) = this
  def errorDetails = Some("No results")
}

// TODO(vlad): stringify better
case class BadResultException(errors: Traversable[String]) extends Exception {
  override def getMessage: String = {
    "errors: " + (errors mkString "; ")
  }
}

object Result {
  type predicate[-P] = Function1[P, Boolean]

  def apply[T](optT: Option[T]): Result[T] = optT map (t => Good(t)) getOrElse NoResult
  def apply[T](optT: Option[T], onError: => String): Result[T] = optT map (t => Good(t)) getOrElse Bad(onError::Nil)

  def forValue[T](value: T, onError: => String): Result[T] = apply(Option(value), onError)

  def forValue[T](value: T): Result[T] = Result(Option(value)) // have to give it a different name, so that it's not mixed with Option

  def apply[T](value: T, p: predicate[T], message: String): Result[T] =
    if (p(value)) Good(value) else error(message)

  def error[T](message: => Any): Bad[T] = Bad(("" + message)::Nil)

  def exception[T](x: Exception, value: => T): Bad[T] = error(s"Exception $x on $value")

  def traverse[T](results: Traversable[Result[T]]): Result[Traversable[T]] = {
    if (results.isEmpty) NoResult else {
      val (goodOnes, notGoodOnes) = results.partition(_.isGood)
      val goodValues = goodOnes collect {case Good(x) => x}
      if (notGoodOnes.isEmpty) Good(goodValues)
      else {
        val (emptyOnes, badOnes) = notGoodOnes partition(NoResult ==)
        if (badOnes.isEmpty) error("Some results are missing")
        else                 Bad(badOnes.toList.flatMap(_.listErrors))
      }
    }
  }
}
