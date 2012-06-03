package scalakittens
package applicative

trait Applicative[T[_]] extends Functor[T] {
  def pure[A](a: A): T[A]

//  the following is the formal declaration
//  def ap[A, B](tf: T[A => B]): T[A] => T[B]

// below are additional enrichments, not exactly pertaining to Applicative

  trait Applicable[A, B] { def <*>(ta: T[A]): T[B] }

  implicit def applicable[A, B](tf: T[A => B]): Applicable[A, B]

  def ap[A, B](fs: T[A => B]) = (ta: T[A]) => fs <*> ta

  trait Lifted[A, B, T[_]] { def <@>(ta: T[A]): T[B] }

  implicit def lift[A, B](fun: A => B) = new Lifted[A, B, T] {
    def <@>(ta: T[A]) = pure(fun) <*> ta
  }
}
