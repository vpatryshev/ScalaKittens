package scalakittens.applicative

import language.{higherKinds, implicitConversions}

/**
 * This is an educational version; for the right solution
 * @see https://github.com/scalaz/scalaz/blob/master/core/src/main/scala/scalaz/Functor.scala
 *
 * @tparam T the parametric type
 */
abstract class ConstantFunctor[T[_]] extends Functor[T] {
  def f1[A, B](f: (A) ⇒ B) = (identity[T[Any]]_).asInstanceOf[T[A] ⇒ T[B]]
}
