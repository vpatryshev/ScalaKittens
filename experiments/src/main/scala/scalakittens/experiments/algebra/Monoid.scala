package scalakittens.experiments.algebra

import scalakittens.experiments.applicative._

import scala.language.{higherKinds, implicitConversions}

trait Monoid[X] extends Semigroup[X] {
  val _0: X

  object App extends ConstantFunctor[Value] with Applicative[Value] {

    override def pure[A](a: A) = value[A](_0)

    override implicit def applicable[A, B](ff: Value[A => B]): Applicable[A, B, Value] = new Applicable[A, B, Value] {
      def <*>(fa: Value[A]) = Value(ff + fa.value)
    }
  }

  def accumulate[A, T[_]](traversable: Traversable[T])(eval: A => X)(ta: T[A]): X = {
    val evalAndWrap: A => Value[X] = eval andThen value
    traversable.traverse(App)(evalAndWrap)(ta).value
  }
}
