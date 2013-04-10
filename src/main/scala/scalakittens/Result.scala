package scalakittens

sealed trait Result[+T] {
  def isGood: Boolean
  def isBad:  Boolean = !isGood
  val listErrors: Traversable[ErrorDetails]
  def onError(op: Traversable[ErrorDetails] => Unit)
  def apply(): T
  def map[U](f: T => U): Result[U]
  def flatMap[U](f: T => Result[U]): Result[U]
  def collect[U](pf: PartialFunction[T, U], onError: => ErrorDetails): Result[U]
  def toOption: Option[T]
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1]
  def getOrElse[T1 >: T](alt: => T1): T1
  def <*>[U](other: Result[U]): Result[(T,U)]
  def foreach(f: (T) ⇒ Unit): Unit
  def filter(p: T => Boolean, errorMessage: => String): Result[T]
  def errorDetails: Option[ErrorDetails]
}

final case class Good[T](value: T) extends Result[T] {
  require(value != null)
  override def isGood = true
  override val listErrors: Traversable[ErrorDetails] = Nil
  def onError(op: Traversable[ErrorDetails] => Unit): Unit = {}
  def apply() = value
  def toOption = Some(value)
  def map[U](f: T=>U) = Good(f(value))
  def flatMap[U](f: T => Result[U]) = f(value)
  def collect[U](pf: PartialFunction[T, U], onError: => ErrorDetails) = Result(pf.lift(value), onError)
  override def toString = "Good(" + value.toString + ")"
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1] = this
  def getOrElse[T1 >: T](alt: => T1): T1 = value
  def <*>[U](other: Result[U]): Result[(T, U)] = other.flatMap(u => Good((value, u)))
  def foreach(f: (T) => Unit) {f(value)}
  def filter(p: T => Boolean, errorMessage: => String) = if (p(value)) this else Result.error(value, errorMessage)
  def errorDetails = None
}

final case class Bad[T](listErrors: Traversable[ErrorDetails]) extends Result[T] {
  def isGood = false
  def onError(op: Traversable[ErrorDetails] => Unit): Unit = {op(listErrors)}
  def apply() = throw new BadResultException(listErrors)
  def toOption = None
  def map[U](f: T=>U) = Bad(listErrors)
  def flatMap[U](f: T => Result[U]) = Bad(listErrors)
  def collect[U](pf: PartialFunction[T, U], onError: => ErrorDetails) = Bad(listErrors)
  def orElse[T1 >: T] (next: => Result[T1]): Result[T1] = next
  def getOrElse[T1 >: T](alt: => T1): T1 = alt
  def <*>[U](other: Result[U]): Result[(T, U)] = Bad(listErrors ++ other.listErrors)

  def foreach(f: T => Unit) {}
  def filter(p: T => Boolean, errorMessage: => String) = this
  def errorDetails = Some(summary)

  private def abbr(x: Any) = {
    val full = "" + x
    if (full.length < 100) full else (full.substring(0, 100) + "...")
  }

  private def summary = ErrorDetails(listErrors map (_.description) mkString "\n", listErrors.flatMap (_.bad).toArray:_*)

  override def toString = summary.description + "\n in: " + abbr(summary.bad)

}

final case object NoResult extends Result[Nothing] {
  def isGood: Boolean = false
  val listErrors: Traversable[ErrorDetails] = Nil
  def onError(op: (Traversable[ErrorDetails]) => Unit) {}
  def apply() = throw new BadResultException(ErrorDetails("No results available")::Nil)
  def map[U](f: Nothing => U): Result[U] = NoResult
  def flatMap[U](f: Nothing => Result[U]): Result[U] = NoResult
  def collect[U](pf: PartialFunction[Nothing, U], onError: => ErrorDetails) = NoResult
  def toOption: Option[Nothing] = None

  def getOrElse[T](alt: => T): T = alt
  def orElse[T1 >: Nothing] (next: => Result[T1]): Result[T1] = next

  def <*>[U](other: Result[U]): Result[(Nothing, U)] = NoResult
  def foreach(f: (Nothing) => Unit) {}
  def filter(p: Nothing => Boolean, errorMessage: => String) = this
  def errorDetails = Some(ErrorDetails("No results"))
}

// TODO(vlad): stringify better
case class BadResultException(errors: Traversable[ErrorDetails]) extends Exception {
  override def getMessage: String = {
    "errors: " + (errors mkString "; ")
  }
}

case class ErrorDetails(description: String, bad: Any*) {
  require (description != null)
  override def toString = "Error: " + description
}

object Result {
  type predicate[T] = T=>Boolean
  def apply[T](optT: Option[T]): Result[T] = optT map (t => Good(t)) getOrElse NoResult
  def apply[T](optT: Option[T], onError: => ErrorDetails): Result[T] = optT map (t => Good(t)) getOrElse Bad(onError::Nil)

  def forValue[T](value: T, onError: => String): Result[T] = apply(Option(value), ErrorDetails(onError, value))

  def forValue[T](value: T): Result[T] = Result(Option(value)) // have to give it a different name, so that it's not mixed with Option

  def apply[T](value: T, p: predicate[T], message: String): Result[T] =
    if (p(value)) Good(value) else error(value, message)

  // todo: remove
  def check[T](p: predicate[T], value: T, errMsg: String = "failed") = apply(value, p, errMsg)

  def error[T](value: T, message: String, args: Any*): Bad[T] = Bad(ErrorDetails(message.format(args:_*), value)::Nil)

  def traverse[T](results: Traversable[Result[T]]): Result[Traversable[T]] = {
    if (results.isEmpty) NoResult else {
      val (goodOnes, notGoodOnes) = results.partition(_.isGood)
      val goodValues = goodOnes collect {case Good(x) => x}
      if (notGoodOnes.isEmpty) Good(goodValues)
      else {
        val (emptyOnes, badOnes) = notGoodOnes partition(NoResult ==)
        if (badOnes.isEmpty) error(goodValues, "Some results are missing")
        else                 Bad(badOnes.toList.flatMap(_.listErrors))
      }
    }
  }
}