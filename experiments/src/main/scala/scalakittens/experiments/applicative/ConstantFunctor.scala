package scalakittens.experiments.applicative

import scalakittens.experiments.applicative.All.F1

import scala.language.higherKinds

/**
 * This is an educational version; for the right solution
 *
 * @see https://github.com/scalaz/scalaz/blob/master/core/src/main/scala/scalaz/Functor.scala
 * @tparam T the parametric type
 */
abstract class ConstantFunctor[T[_]] extends Functor[T] {
  def f1[A, B](f: (A) => B): F1[T, A, B] =
    (identity[T[Any]] _).asInstanceOf[T[A] => T[B]]
}
