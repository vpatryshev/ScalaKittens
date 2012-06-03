package scalakittens
package applicative

trait Traversable[T[_]] {
  def traverse[A, B, F[_]](app: Applicative[F])(f: A => F[B])(as: T[A]): F[T[B]]
  def dist[B, F[_]](app: Applicative[F]) = traverse[F[B], B, F](app)(identity[F[B]]) _
}
