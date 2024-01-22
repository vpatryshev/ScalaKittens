package scalakittens.experiments.algebra

import scala.language.implicitConversions

trait Semigroup[X] {
  def add(x: X, y: X): X

  case class Value[A](value: X) {
    def +(another: X): X = add(value, another)
  }

  implicit def value[A](x: X): Value[A] = Value(x)

}
