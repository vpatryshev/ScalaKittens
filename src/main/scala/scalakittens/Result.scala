package scalakittens

sealed trait Result[T] {
  /**
   * Not recommended
   * @return true if it's good
   */
  def isGood: Boolean
  /**
   * Not recommended
   * @return true if it's bad
   */
  def isBad:  Boolean = !isGood
  val listErrors: Seq[ErrorDetails]
  def onError(op: Seq[ErrorDetails] => Unit)
  def apply(): T
  def map[U](f: T => U): Result[U]
  def flatMap[U](f: T => Result[U]): Result[U]
}

final case class Good[T](value: T) extends Result[T] {
  require(value != null)
  override def isGood = true
  override val listErrors: Seq[ErrorDetails] = Nil
  override def onError(op: Seq[ErrorDetails] => Unit): Unit = {}
  def apply() = value
  def map[U](f: T=>U) = Good(f(value))
  def flatMap[U](f: T => Result[U]) = f(value)
  override def toString = value.toString
}

final case class Bad[T](listErrors: Seq[ErrorDetails]) extends Result[T] {
  override def isGood = false
  override def onError(op: Seq[ErrorDetails] => Unit): Unit = {op(listErrors)}
  def apply() = throw new BadResultException[T](listErrors)
  def map[U](f: T=>U) = Bad(listErrors)
  def flatMap[U](f: T => Result[U]) = Bad(listErrors)
}

// TODO(vlad): stringify better maybe
case class BadResultException[T](errors: Seq[ErrorDetails]) extends Exception {
  override def getMessage: String = {
    "errors: " + (errors mkString "; ")
  }
}

case class ErrorDetails(description: String, bad: Any) {
  require (description != null)
  override def toString = "Error: " + description + " in " + bad
}

object Result {
  def apply[T](optT: Option[T], onError: => ErrorDetails): Result[T] = optT match {
    case Some(t) => Good(t)
    case none    => Bad(onError::Nil)
  }

  def apply[T](value: T, predicate: T => Boolean, message: String): Result[T] =
    if (predicate(value)) Good(value) else Bad(ErrorDetails(message, value)::Nil)
}