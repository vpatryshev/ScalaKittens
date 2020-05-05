package scalakittens.experiments.applicative

import scala.language.{higherKinds, implicitConversions}

trait Applicative[T[_]] extends Functor[T] {
  self ⇒
  def pure[A](a: A): T[A]

  // below are additional enrichments, not exactly pertaining to Applicative
  def ap[A, B](fs: T[A ⇒ B]): (T[A] ⇒ T[B]) = (ta: T[A]) ⇒ fs <*> ta

  implicit def applicable[A, B](tf: T[A ⇒ B]): Applicable[A, B, T]

  trait Lifted[A, B] {def <@>(ta: T[A]): T[B]}

  implicit def lift[A, B](fun: A ⇒ B): Lifted[A, B] = new Lifted[A, B] {
    def <@>(ta: T[A]) = pure(fun) <*> ta
  }

  // the following line has no error; there's an intellij bug that thinks it is
  def andThen[U[_]](u: Applicative[U]) = new Applicative[({type UT[X] = U[T[X]]})#UT] {
    type UT[X] = U[T[X]]

    def f1[A, B](f: A ⇒ B): (U[T[A]]) ⇒ U[T[B]] = u.f1(self.f1(f))

    def pure[A](a: A): U[T[A]] = u.pure(self.pure(a))

    /**
     * Comments from sassa_nf@livejournal.com
     * u.ap is U[W⇒Z]⇒U[W]⇒U[Z]
     * u.ap is U[X⇒Y]⇒U[X]⇒U[Y]
     * u.pure is C⇒U[C]
     * self.ap is T[A⇒B]⇒T[A]⇒T[B]
     *
     * so, u.pure is T[A⇒B]⇒T[A]⇒T[B] ⇒ U[T[A⇒B]⇒T[A]⇒T[B]]
     *
     * and inner u.ap is U[T[A⇒B]⇒T[A]⇒T[B]]⇒U[T[A⇒B]]⇒U[T[A]⇒T[B]]
     *
     * and outer u.ap is U[T[A]⇒T[B]]⇒U[T[A]]⇒U[T[B]]
     */

    implicit def applicable[A, B](utf: U[T[A ⇒ B]]): Applicable[A, B, UT] = {
      val uta2tb: U[(T[A]) ⇒ T[B]] = u.f1(self.ap[A, B])(utf)
      new Applicable[A, B, UT] {
        def <*>(uta: UT[A]): U[T[B]] = u.applicable(uta2tb) <*> uta
      }
    }
  }
}

trait Applicable[A, B, T[_]] {def <*>(ta: T[A]): T[B]}

