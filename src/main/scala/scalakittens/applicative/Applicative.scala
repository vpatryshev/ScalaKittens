package scalakittens
package applicative

trait Applicative[T[_]] extends Functor[T] { self =>
  def pure[A](a: A): T[A]

//  the following is the formal declaration
//  def ap[A, B](tf: T[A => B]): T[A] => T[B]

// below are additional enrichments, not exactly pertaining to Applicative

  implicit def applicable[A, B](tf: T[A => B]): Applicable[A, B, T]

  def ap[A, B](fs: T[A => B]) = (ta: T[A]) => fs <*> ta

  trait Lifted[A, B, T[_]] { def <@>(ta: T[A]): T[B] }

  implicit def lift[A, B](fun: A => B) = new Lifted[A, B, T] {
    def <@>(ta: T[A]) = pure(fun) <*> ta
  }

  def andThen[U[_]](u: Applicative[U]) = new Applicative[({type λ[α] = U[T[α]]})#λ] {
    def f1[A, B](f: A => B): (U[T[A]]) => U[T[B]] = u.f1(self.f1(f)) // have to figure out how not to repeat

    def pure[A](a: A) : U[T[A]] = u.pure(self.pure(a))

// self.ap transforms T[A=>B] to T[A] => T[B]
// u.f1(self.ap) will transform U[T[A=>B]] to U[T[A] => T[B]]
// u.ap will transform the last one to U[T[A]] => U[T[B]]
    type λ[α] = U[T[α]]
    implicit def applicable[A, B](utf: U[T[A => B]]): Applicable[A,  B, λ] = {
      val uta2tb: U[(T[A]) => T[B]] = u.f1(self.ap[A, B])(utf)
      new Applicable[A, B, λ] {
        def <*>(uta: λ[A]) = u.applicable(uta2tb) <*> uta
      }
    }
  }
}

trait Applicable[A, B, T[_]] { def <*>(ta: T[A]): T[B] }

