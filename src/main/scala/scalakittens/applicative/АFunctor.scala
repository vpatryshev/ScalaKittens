package scalakittens
package applicative

trait Functor[T[_]] { self =>
  // mapping on objects of a category
  type f0[_] = T[_]

  // mapping on arrows of a category
  def f1[A, B](f: A => B): T[A] => T[B]

  // the following thing we need for some obscure scala reason
  implicit def as[A](t: f0[A]) = t.asInstanceOf[T[A]]

  // builds a composition of two functors
  def andThen[U[_]](u: Functor[U]) = new Functor[({type UT[X] = U[T[X]]})#UT] {
    def f1[A, B](f: A => B): (U[T[A]]) => U[T[B]] = u.f1(self.f1(f))
  }
}

object Base { // todo: figure out how it might work
  type Comp[F[_], G[_]] = ({type λ[α] = G[F[α]]})#λ[_]
}
