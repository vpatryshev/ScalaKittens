package scalakittens.experiments

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

// source: http://infoscience.epfl.ch/record/150280/files/TypeClasses.pdf
// authors: Bruno C. d. S. Oliveira, Adriaan Moors, Martin Odersky
object TypeClass {

  trait Ord[T] {
    def compare(a: T, b: T): Boolean
  }

  implicit object IntOrd extends Ord[Int] {
    def compare(a: Int, b: Int): Boolean = a <= b
  }

  // TODO: get rid of warning
  def sort[T](xs: List[T])(implicit ordT: Ord[T]): List[T] = {
    println(s"well, at least it compiles, $ordT");
    Nil
  }

  val sorted: scala.collection.immutable.List[Int] = sort(List(1, 2, 3))

  trait Monoid[A] {
    def binary_op(x: A, y: A): A

    def identity: A
  }

  def acc[A](l: List[A])(implicit m: Monoid[A]): A =
    l.foldLeft(m.identity)((x, y) => m.binary_op(x, y))

  object A {

    implicit object sumMonoid extends Monoid[Int] {
      def binary_op(x: Int, y: Int): Int = x + y

      def identity = 0
    }

    def sum(l: List[Int]): Int = acc(l)
  }

  object B {

    implicit object prodMonoid extends Monoid[Int] {
      def binary_op(x: Int, y: Int): Int = x * y

      def identity = 1
    }

    def product(l: List[Int]): Int = acc(l)
  }

  val test1: (Int, Int, Int) = {
    import A._
    import B._
    val l = List(1, 2, 3, 4, 5)
    (sum(l), product(l), acc(l)(prodMonoid))
  }

  trait Functor[F[_]] {
    def map[X, Y](f: X => Y)(fx: F[X]): F[Y]
  }

  implicit def fops[F[_] : Functor, A](fa: F[A]): Object {
    def map[B](f: A => B): F[B]

    val functor: Functor[F]
  } = new {
    val functor: Functor[F] = implicitly[Functor[F]]

    final def map[B](f: A => B): F[B] = functor.map(f)(fa)
  }

  import java.util._

  implicit object ArrayList_is_a_Functor extends Functor[ArrayList] {
    def map[X, Y](f: X => Y)(listX: ArrayList[X]): ArrayList[Y] = {
      val listY = new ArrayList[Y]
      for (i ← 0 to listX.size) listY.add(f(listX.get(i)))
      listY
    }
  }

  val testList = new ArrayList[String](Arrays.asList("this", "is", "a", "test"))
  val transformed: ArrayList[String] = testList.map(_.toUpperCase)
}
