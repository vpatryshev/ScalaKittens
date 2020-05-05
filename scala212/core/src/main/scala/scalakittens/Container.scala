package scalakittens

/**
  * A privitive idea of a container; can be empty.
  * Created by vpatryshev on 10/18/15.
  */
trait Container[+T] extends Goodness {
  def isEmpty: Boolean
  def nonEmpty = !isEmpty
}

trait NothingInside[+T] extends Container[T] with NegativeAttitude {
  def isEmpty = true
}

trait SomethingInside[+T] extends Container[T] with PositiveAttitude {
  def isEmpty = false
}
