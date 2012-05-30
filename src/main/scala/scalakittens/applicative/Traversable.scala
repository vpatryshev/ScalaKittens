package scalakittens
package applicative

trait Traversable[T[_]] {
  def traverse0[A, B, F[+_]](app: Applicative[F])(f: A => F[B])(as: T[A]): F[T[B]]

  def traverse[A, B, F[+_]](implicit app: Applicative[F]) = traverse0[A, B, F](app) _

  def dist0[B, F[+_]](app: Applicative[F])(tfa: T[F[B]]) = traverse0[F[B], B, F](app)(identity[F[B]]) _

  def dist[B, F[+_]](implicit app: Applicative[F]) = dist0[B, F](app) _
}
