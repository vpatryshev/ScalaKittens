package scalakittens
package applicative

trait Functor[T[_]] {
  // mapping on objects of a category
  type f0[_] = T[_]

  // mapping on arrows of a category
  def f1[A, B](f: A => B): T[A] => T[B]

  // the following thing we need for some obscure scala reason
  implicit def as[A](t: f0[A]) = t.asInstanceOf[T[A]]
}
