package scalakittens

import java.util

// source: http://infoscience.epfl.ch/record/150280/files/TypeClasses.pdf
// authors: Bruno C. d. S. Oliveira, Adriaan Moors, Martin Odersky
object TypeClass {
  trait Monoid [A] {
    def binary_op (x :A,y :A):A
    def identity :A
  }
  def acc [A] (l :List[A]) (implicit m:Monoid [A]):A =
    l.foldLeft (m.identity) ((x,y) ⇒ m.binary_op (x,y))
  object A {
    implicit object sumMonoid extends Monoid [Int] {
      def binary_op (x :Int,y :Int) = x+y
      def identity = 0
    }
    def sum (l :List[Int]):Int = acc (l)
  }
  object B {
    implicit object prodMonoid extends Monoid [Int] {
      def binary_op (x :Int,y :Int) = x * y
      def identity = 1
    }
    def product (l :List[Int]):Int = acc (l)
  }

  val test :(Int,Int,Int) = {
    import A._
    import B._
    val l = List (1,2,3,4,5)
    (sum (l),product (l),acc (l) (prodMonoid))
  }

  //Source: myself mostly; and Josh Suereth's Scala in Depth
  trait Functor[F[_]] { def map[X,Y](f: X => Y): F[X]=>F[Y] }

  implicit def making_a_functor[F[_]: Functor, A](fa: F[A]) = new {
    final def map[B](f:A=>B) = implicitly[Functor[F]].map(f)
  }

  import java.util.ArrayList

  object ArrayList_is_a_Functor extends Functor[ArrayList] {
      def map[X,Y](f: X => Y) = (xs: ArrayList[X]) => {
        val ys = new ArrayList[Y]
        for (i <- 0 to xs.size) ys.add(f(xs.get(i)))
        ys
      }
    }

  val testList = new ArrayList(Arrays.asList("this", "is", "a", "test"))
  val transformed = testList.map(_.toUpperCase)

}
