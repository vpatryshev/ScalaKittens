package scalakittens.applicative

trait Semigroup[O] {
  def add(x: O, y: O): O

  case class Acc[A](value: O) {
    def <+>(another: O): O = add(value, another)
  }

  implicit def acc[A](x: O): Acc[A] = Acc(x)

}
