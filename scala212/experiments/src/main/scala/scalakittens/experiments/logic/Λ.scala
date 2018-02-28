package scalakittens.experiments.logic

/**
  * Experiments with logic and typed/untyped lambda implemented in Scala
  * Created by vpatryshev on 12/10/14.
  */
trait λ[T] {

}

case class Var[T](name: String) extends λ[T]
case class App[U,V](left: λ[U⇒V], V:λ[U]) extends λ[V]
case class Abs[U,V](x: λ[U⇒V]) extends λ[U⇒V]

object UntypedSKI {
  val I = (x:Any) ⇒ x
  val K = (x:Any) ⇒ (y: Any) ⇒ x
  val S = (x:Any⇒Any⇒Any) ⇒ (y: Any⇒Any) ⇒ (z:Any) ⇒ x(z)(y(z))
// the following won't compile
//  val Y = S (K (S(I)(I))) (S (S (K(S)) K) (K (S(I)(I))))

  def main (args: Array[String]) {
    val SKK = S(K)(K)
    println(SKK(1))
    println(SKK("Hello Lambda"))
  }
}

// https://gist.github.com/othiym23/1034029 - typed SKI
object TypedSKI {
  def I[A]:(A⇒A) = (x:A) ⇒ x
  def K[A,B]:(A⇒B⇒A) = (x:A) ⇒ (y: B) ⇒ x
  def S[A,B,C]:(A ⇒ B ⇒ C) ⇒ (A ⇒ B) ⇒ (A ⇒ C) =
    (x:A⇒B⇒C) ⇒ (y: A⇒B) ⇒ (z:A) ⇒ x(z)(y(z))
  def si[A,B]:(((A⇒B)⇒A)⇒((A⇒B)⇒B)) = S[A⇒B,A,B](I[A⇒B])
//  def sii[A,B] = si[A,B](I[(A⇒B)⇒A])
//  def Y = S (K (S(I)(I))) (S (S (K(S)) (K)) (K (S(I)(I))))

  def main (args: Array[String]) {
    def sk[A,B] = {
      val functionToFunction: (A ⇒ B) ⇒ A ⇒ A = S[A, B, A](K[A, B])
      functionToFunction
    }
    def SKK[A,B] = S[A,B⇒A,A](K[A,B⇒A])(K[A,B])
    println(SKK(1))
    println(SKK("Hello Lambda"))
    print(sk[Double, String])
  }
}
