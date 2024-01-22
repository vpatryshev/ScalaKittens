package scalakittens.experiments.applicative

import scala.language.higherKinds

trait Traversable[T[_]] {
  def traverse[A, B, F[_]](app: Applicative[F])(f: A => F[B])(as: T[A]): F[T[B]]

  def dist[B, F[_]](app: Applicative[F]): T[F[B]] => F[T[B]] =
    traverse[F[B], B, F](app)(identity[F[B]])
}
