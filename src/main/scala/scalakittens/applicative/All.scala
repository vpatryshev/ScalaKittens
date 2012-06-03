// http://hseeberger.wordpress.com/2011/01/31/applicatives-are-generalized-functors/
// Tony Morris, sassa_nf, Miles Sabin (shapeless project)
// McBride, Paterson
// Wadler (theorems for free)

// check out: Gunnar, Kleisli, monads for free - for dependency injection, on youtube
package scalakittens
package applicative

import scala.collection._
import immutable.List

object All {

  /**
   * Functorial features of List type
   */
  trait ListFunctor extends Functor[List] {
    override def f1[A, B](f: A => B) = (aa: List[A]) =>  aa map f
  }

  /**
   * Functorial features of Seq type
   */
  trait SeqFunctor extends Functor[Seq] {
    override def f1[A, B](f: A => B) = (aa: Seq[A]) => aa map f
  }

  /**
   * Functorial features of Set type (covariant version)
   */
  trait SetFunctor extends Functor[Set] {
    override def f1[A, B](f: A => B) = (sa: Set[A]) => sa map f
  }

  object AppList extends ListFunctor with Applicative[List] {
    override def pure[A](a: A) = List(a)

    override implicit def applicable[A, B](lf: List[A => B]): Applicable[A, B, List] = {
      new Applicable[A, B, List] {
        def <*>(as: List[A]) = (for (f <- lf; a <- as) yield f(a)).toList
      }
    }
  }

  object AppSet extends SetFunctor with Applicative[Set] {
    override def pure[A](a: A) = Set(a)

    override implicit def applicable[A, B](ff: Set[A => B]) = {
      val sf: Set[A => B] = ff
      new Applicable[A, B, Set] {
        def <*>(fa: Set[A]) = {
          (for (f <- sf; a <- fa) yield f(a)).toSet
        }
      }
    }
  }


  object AppZip extends SeqFunctor with Applicative[Seq] {

    override def pure[A](a: A): Seq[A] = Stream.continually(a)

    implicit def applicable[A, B](ff: Seq[A => B]) = {
      new Applicable[A, B, Seq] {
        def <*>(az: Seq[A]) = ff zip az map ({ case (f: (A => B), a: A) => f(a) })
      }
    }
  }


  // combinators
  implicit def ski[E, A, B](fe: E => A => B) = new {
    val S = (ae: E => A) => (env: E) => fe(env)(ae(env))
  }

  def K[Env, A](x: A) = (gamma: Env) => x

  type E = Any => Int

  type Env[X] = E => X

  trait EnvFunctor extends Functor[Env] {

    def asEnv[A](fx: Env[A]) = fx.asInstanceOf[Env[A]]

    override def f1[A, B](f: A => B) = (aa: Env[A]) => asEnv(aa) andThen f
  }

  def KEnv[X](x: X) = K[E, X](x)

  trait AppEnv extends Applicative[Env] {

    implicit def applicable[A, B](fe: Env[A => B]) = new Applicable[A, B, Env] {
      def <*>(fa: Env[A]) = ski(fe) S fa
    }

    def pure[A](a: A): Env[A] =KEnv(a)
    override def ap[A, B](fe: Env[A => B]) = applicable(fe) <*> _
  }

  //def FOLD[E, A, B, F <: E =>A =>B, P](f: F, p: (E => P)*) = {
  //  (K(f))(p) /: ((x: E=>F, y: E => B) => ski(x) S y)
  //}

  implicit object TraversableList extends scalakittens.applicative.Traversable[List] {
    def cons[A](a: A)(as: List[A]): List[A] = a :: as

    def traverse0[A, B, F[_]](app: Applicative[F])(f: A => F[B])(al: List[A]): F[List[B]] = {
      al match {
        case Nil => app.pure(List[B]())
        case head :: tail => app.applicable(app.lift(cons[B] _) <@> f(head)) <*> traverse0[A, B, F](app)(f)(tail)
      }
    }
  }

  trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  def leaf[T](t: T): Tree[T] = Leaf(t)
  case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  def node[T](left: Tree[T])(right: Tree[T]): Tree[T] = Node(left, right)

  implicit object TraversableTree extends scalakittens.applicative.Traversable[Tree] {

    def traverse0[A, B, F[_]](app: Applicative[F])(f: A => F[B])(at: Tree[A]): F[Tree[B]] = at match {

      case Leaf(a) => app.lift(leaf[B]) <@> f(a)
      case Node(left, right) => {
        implicit def applicable[A, B](tf: F[A => B]) = app.applicable(tf)

        val traverse: (Tree[A]) => F[Tree[B]] = traverse0(app)(f)

        app.pure(node[B] _) <*> traverse(left) <*> traverse(right)
      }
    }
  }
}
