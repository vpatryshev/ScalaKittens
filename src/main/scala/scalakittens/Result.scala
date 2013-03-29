package scalakittens


sealed trait Result[T] {
  def isGood: Boolean
  def isBad:  Boolean = !isGood
  val listErrors: Seq[ErrorDetails]
  def onError(op: Seq[ErrorDetails] => Unit)
  def apply(): T
  def map[U](f: T => U): Result[U]
  def flatMap[U](f: T => Result[U]): Result[U]
  def toOption: Option[T]
  def orElse(next: => Result[T]): Result[T]
  def getOrElse(alt: => T): T
  def <*>[U](other: Result[U]): Result[(T,U)]
  def foreach(f: (T) ⇒ Unit): Unit
}

final case class Good[T](value: T) extends Result[T] {
  require(value != null)
  override def isGood = true
  override val listErrors: Seq[ErrorDetails] = Nil
  def onError(op: Seq[ErrorDetails] => Unit): Unit = {}
  def throwOnError {}
  def apply() = value
  def toOption = Some(value)
  def map[U](f: T=>U) = Good(f(value))
  def flatMap[U](f: T => Result[U]) = f(value)
  override def toString = value.toString
  def orElse(next: => Result[T]) = this
  def getOrElse(alt: => T) = value
  def <*>[U](other: Result[U]): Result[(T, U)] = other.flatMap(u => Good((value, u)))
  def foreach(f: (T) => Unit) {f(value)}
}

final case class Bad[T](listErrors: Seq[ErrorDetails]) extends Result[T] {
  def isGood = false
  def onError(op: Seq[ErrorDetails] => Unit): Unit = {op(listErrors)}
  def apply() = throw new BadResultException[T](listErrors)
  def toOption = None
  def map[U](f: T=>U) = Bad(listErrors)
  def flatMap[U](f: T => Result[U]) = Bad(listErrors)
  def orElse(next: => Result[T]) = next
  def getOrElse(alt: => T) = alt
  def <*>[U](other: Result[U]): Result[(T, U)] = Bad(listErrors ++ other.listErrors)

  def foreach(f: (T) => Unit) {}
  private def abbr(x: Any) = {
    val full = "" + x
    if (full.length < 100) full else (full.substring(0, 100) + "...")
  }
  override def toString = listErrors.mkString("\n") + "\n in: " + abbr(listErrors.head.bad)
}

final case class NoResult[T]() extends Result[T] {
  def isGood: Boolean = false
  val listErrors: Seq[ErrorDetails] = Nil
  def onError(op: (Seq[ErrorDetails]) => Unit) {}
  def map[U](f: (T) => U): Result[U] = NoResult[U]
  def flatMap[U](f: (T) => Result[U]): Result[U] = NoResult[U]
  def toOption: Option[T] = None
  def orElse(next: => Result[T]): Result[T] = next
  def getOrElse(alt: => T): T = alt
  def <*>[U](other: Result[U]): Result[(T, U)] = NoResult[(T, U)]

  def foreach(f: (T) => Unit) {}
}

// TODO(vlad): stringify better
case class BadResultException[T](errors: Seq[ErrorDetails]) extends Exception {
  override def getMessage: String = {
    "errors: " + (errors mkString "; ")
  }
}

case class ErrorDetails(description: String, bad: Any) {
  require (description != null)
  override def toString = "Error: " + description
}

object Result {
  type predicate[T] = T=>Boolean

  def apply[T](optT: Option[T], onError: => ErrorDetails): Result[T] = optT match {
    case Some(t) => Good(t)
    case none    => Bad(onError::Nil)
  }

  def apply[T](value: T, p: predicate[T], message: String): Result[T] =
    if (p(value)) Good(value) else error(value, message)

  def check[T](p: predicate[T], value: T) = apply(value, p, "failed")

  def error[T](value: T, message: String, args: Any*): Bad[T] = Bad(ErrorDetails(message.format(args:_*), value)::Nil)
}