package scalakittens.experiments.algebra

object ScalazMonoid {

  trait Monoid[A] {
    def mappend(a1: A)(a2: A): A

    def mzero: A

    def op(a1: A, a2: A) = mappend(a1)(a2)
  }

  /*
      implicit def CanAdd[A:Monoid](a:A) = new {
        val m = implicitly[Monoid[A]]
        def +(b:A) = m.mappend(a,b)
      }
  */
  object Monoid {
    implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
      def mappend(a: Int)(b: Int): Int = a + b

      def mzero: Int = 0
    }
    implicit val StringMonoid: Monoid[String] = new Monoid[String] {
      def mappend(a: String)(b: String): String = a + b

      def mzero: String = ""
    }
  }

  def sum[A: Monoid](xs: List[A]): A = {
    val m = implicitly[Monoid[A]]
    xs.foldLeft(m.mzero)(m.op)
  }
}
