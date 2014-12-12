package scalakittens.logic

/**
 * Created by vpatryshev on 12/10/14.
 */
trait Lambda[T] {

}

case class Var[T](name: String) extends Lambda[T]
case class App[U,V](left: Lambda[U=>V], V:Lambda[U]) extends Lambda[V]
case class Abs[U,V](x: Lambda[U=>V]) extends Lambda[U=>V]

object SKI {
  val I = (x:Any) => x
  val K = (x:Any) => (y: Any) => x
  val S = (x:Any=>Any=>Any) => (y: Any=>Any) => (z:Any) => x(z)(y(z))

  def main (args: Array[String]) {
    val SKK = S(K)(K)
    println(SKK(1))

  }
}
