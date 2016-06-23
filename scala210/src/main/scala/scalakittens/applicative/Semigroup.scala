package scalakittens.applicative
import language.{higherKinds, implicitConversions}

trait Semigroup[X] {
  def add(x: X, y: X): X

  case class Value[A](value: X) {
    def +(another: X): X = add(value, another)
  }

  implicit def value[A](x: X): Value[A] = Value(x)

}
