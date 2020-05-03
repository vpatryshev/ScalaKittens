// http://hseeberger.wordpress.com/2011/01/31/applicatives-are-generalized-functors/
// Tony Morris, sassa_nf, Miles Sabin (shapeless project)
// McBride, Paterson
// Wadler (theorems for free)

// check out: Gunnar, Kleisli, monads for free - for dependency injection, on youtube
package scalakittens.experiments.applicative

import scala.collection._
import scala.collection.immutable.List
import scala.language.{higherKinds, implicitConversions}
import scalakittens.experiments.algebra.{Monoid, Semigroup}

object All {
  type F1[F[_],X,Y] = F[X]⇒F[Y]
  
  /**
    * Functorial features of List type
    */
  trait ListFunctor extends Functor[List] {
    override def f1[A, B](f: A ⇒ B): F1[List,A,B] = (aa: List[A]) ⇒  aa map f
  }

  /**
    * Functorial features of Seq type
    */
  trait SeqFunctor extends Functor[Seq] {
    override def f1[A, B](f: A ⇒ B): F1[Seq,A,B] = (aa: Seq[A]) ⇒ aa map f
  }

  /**
    * Functorial features of Set type (covariant version)
    */
  trait SetFunctor extends Functor[Set] {
    override def f1[A, B](f: A ⇒ B): F1[Set,A,B] = (sa: Set[A]) ⇒ sa map f
  }

  /**
    * Functorial features of Either[X,Y], for the second parameter
    */
  trait RightEitherFunctor[L] extends Functor[({type Maybe[A] = Either[L, A]})#Maybe] {
    def f1[A, B](f: A ⇒ B): Either[L, A] ⇒ Either[L, B] = _.right.map(f)
  }

  object AppList extends ListFunctor with Applicative[List] {
    override def pure[A](a: A) = List(a)

    override implicit def applicable[A, B](lf: List[A ⇒ B]): Applicable[A, B, List] = {
      new Applicable[A, B, List] {
        def <*>(as: List[A]) = for (f <- lf; a <- as) yield f(a)
      }
    }
  }

  object AppSet extends SetFunctor with Applicative[Set] {
    override def pure[A](a: A) = Set(a)

    override implicit def applicable[A, B](ff: Set[A ⇒ B]):Applicable[A, B, Set] = {
      val sf: Set[A ⇒ B] = ff
      new Applicable[A, B, Set] {
        def <*>(fa: Set[A]) = (for (f <- sf; a <- fa) yield f(a)).toSet
      }
    }
  }

  object AppZip extends SeqFunctor with Applicative[Seq] {

    override def pure[A](a: A): Seq[A] = Stream.continually(a)

    implicit def applicable[A, B](ff: Seq[A ⇒ B]): Applicable[A, B, Seq] = {
      new Applicable[A, B, Seq] {
        def <*>(az: Seq[A]) = for {
          (f, a) <- ff zip az
        } yield f(a)
      }
    }
  }

  object EnvExample {

    type Env = Map[String, Int] // String⇒Int would be enough, but for educational purposes...

    type Environmental[X] = Env ⇒ X

    trait EnvFunctor extends Functor[Environmental] {
      override def f1[A, B](f: A ⇒ B) = (aa: Environmental[A]) ⇒ aa andThen f
    }

    implicit def K[A](a: A): (Env ⇒ A) = (env: Env) ⇒ a

    trait HaveS[A, B] { def S: (Env ⇒ A) ⇒ Env ⇒ B }

    // combinators
    implicit def ski[A, B](fe: Env ⇒ A ⇒ B): HaveS[A, B] =
      new HaveS[A, B] {
        val S = (ae: Env ⇒ A) ⇒ (env: Env) ⇒ fe(env)(ae(env))
      }

    trait AppEnv extends EnvFunctor {
      def pure[A](a: A): Environmental[A] = K(a)

      implicit def applicable[A, B](fe: Environmental[A ⇒ B]): Applicable[A, B, Environmental] = new Applicable[A, B, Environmental] {
        def <*>(fa: Environmental[A]) = fe S fa // aka S(f, a)
      }
    }
  }

  //def FOLD[E, A, B, F <: E ⇒A ⇒B, P](f: F, p: (E ⇒ P)*) = {
  //  (K(f))(p) /: ((x: E⇒F, y: E ⇒ B) ⇒ ski(x) S y)
  //}

  implicit object TraversableList extends scalakittens.experiments.applicative.Traversable[List] {
    def cons[A](a: A)(as: List[A]): List[A] = a :: as

    def traverse[A, B, F[_]](app: Applicative[F])(f: A ⇒ F[B])(al: List[A]): F[List[B]] = {
      al match {
        case Nil ⇒ app.pure(List[B]())
        case head :: tail ⇒ app.applicable(app.lift(cons[B]) <@> f(head)) <*> traverse[A, B, F](app)(f)(tail)
      }
    }
  }

  trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  def leaf[T](t: T): Tree[T] = Leaf(t)
  case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  def node[T](left: Tree[T])(right: Tree[T]): Tree[T] = Node(left, right)

  implicit object TraversableTree extends Traversable[Tree] {

    def traverse[A, B, F[_]](app: Applicative[F])(f: A ⇒ F[B])(at: Tree[A]): F[Tree[B]] = at match {

      case Leaf(a) ⇒ app.lift(leaf[B]) <@> f(a)
      case Node(left, right) ⇒
        implicit def applicable[X, Y](tf: F[X ⇒ Y]): Applicable[X,Y,F] = app.applicable(tf)

        val traverse1: (Tree[A]) ⇒ F[Tree[B]] = traverse(app)(f)

        app.pure(node[B] _) <*> traverse1(left) <*> traverse1(right)
    }
  }

  object IntMonoid extends Monoid[Int] {
    val _0 = 0
    def add(x: Int, y: Int) = x + y
  }

  object StringMonoid extends Monoid[String] {
    val _0 = ""
    def add(x: String, y: String) = x + y
  }

  trait ListMonoid[T] extends Monoid[List[T]] {
    val _0 = Nil
    def add(x: List[T], y: List[T]) = x ++ y
  }

  //  val exceptionLog = ListMonoid[Exception]

  trait AccumulatingErrors[Bad] {
    val errorLog: Semigroup[Bad]
    implicit def acc(err: Bad): errorLog.Value[Bad] = errorLog.value(err)
    type Maybe[T] = Either[Bad, T]

    object App extends Applicative[({type Maybe[A] = Either[Bad, A]})#Maybe] with RightEitherFunctor[Bad] {

      def pure[A](a: A):Either[Bad, A] = Right[Bad, A](a)

      implicit def applicable[A, B](maybeF: Either[Bad, A ⇒ B]): Applicable[A, B, Maybe] =
        new Applicable[A, B, Maybe] {

          def <*>(maybeA: Maybe[A]) = (maybeF, maybeA) match {
            case (Left(badF), Left(badA)) ⇒ Left(badF + badA)
            case (Left(badF), _)          ⇒ maybeF
            case (Right(f),  Left(badA))  ⇒ maybeA
            case (Right(f), Right(a))     ⇒ Right(f(a))
          }
        }
    }
  }
}
