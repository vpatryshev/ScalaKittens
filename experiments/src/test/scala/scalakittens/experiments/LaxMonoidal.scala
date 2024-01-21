package scalakittens.experiments

import org.specs2.mutable.Specification

import scala.language.higherKinds

/**
 * Experimenting wiht lax monoidal functors
 * Not clear why it may be usable. But at least it's concise.
 * Created by vpatryshev on 8/15/15.
 */
class LaxMonoidal extends Specification {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(f andThen pure)

    def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

    def ifM[A](fb: F[Boolean])(t: => F[A], f: => F[A]): F[A] = flatMap(fb)(b => if (b) t else f)
  }

  trait Applicative[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]

    def ap[A, B](fa: F[A])(ff: F[A => B]): F[B]

    override def map[A, B](fa: F[A])(f: A => B) = ap(fa)(pure(f))

    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ap(fb)(map(fa)(a => (b: B) => (a, b)))
  }

  trait ApMonad[F[_]] extends Applicative[F] with Monad[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    override def ap[A, B](fa: F[A])(ff: F[A => B]): F[B] = flatMap(ff)(f => map(fa)(f))

    override def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      val coupler: A => B => (A, B) = a => (b: B) => (a, b)
      val liftedCouplerB: F[B => (A, B)] = map(fa)(coupler)
      val lifter: ((B) => (A, B)) => F[(A, B)] = f => map(fb)(f)
      flatMap(liftedCouplerB)(lifter)
    }

    //ap(fb)(map(fa)(a=>(b:B)=>(a,b)))
  }

  case class Lst[T](content: List[T]) extends ApMonad[Lst] {
    override def flatMap[A, B](fa: Lst[A])(f: (A) => Lst[B]): Lst[B] =
      new Lst[B](fa.content.flatMap((a: A) => f(a).content))

    override def pure[A](a: A): Lst[A] = new Lst[A](List(a))
  }

  case class ZLst[T](content: List[T]) extends ApMonad[ZLst] {
    override def flatMap[A, B](fa: ZLst[A])(f: (A) => ZLst[B]): ZLst[B] =
      new ZLst[B](fa.content.flatMap((a: A) => f(a).content))

    override def zip[A, B](fa: ZLst[A], fb: ZLst[B]): ZLst[(A, B)] = ZLst(fa.content.zip(fb.content))

    override def ap[A, B](fa: ZLst[A])(ff: ZLst[A => B]): ZLst[B] = {
      val z: ZLst[(A => B, A)] = zip(ff, fa)
      val zippedList: List[B] = z.content map { p => p._1(p._2) }

      new ZLst(zippedList)
    }

    override def pure[A](a: A): ZLst[A] = new ZLst[A](List(a))
  }

  "lax monoidal" should {
    "behave" in {
      println(new Lst(Nil) zip(new Lst(1 :: 2 :: 3 :: Nil), new Lst('a' :: 'b' :: Nil)))
      println(new ZLst(Nil) zip(new ZLst(1 :: 2 :: 3 :: Nil), new ZLst('a' :: 'b' :: Nil)))
      true
    }
  }
}
