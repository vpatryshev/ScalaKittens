package scalakittens.experiments

import scala.language.higherKinds

/**
 * Created by vpatryshev on 12/5/16.
 *
 * @see https://scu.illiad.oclc.org/illiad/sta/illiad.dll?Action=10&Form=75&Value=91866
 *      Wouter Swierstra
 * @see also //    type x[y] = ({type λ[y] = ...)})#λ
 *      // http://stackoverflow.com/questions/8736164/what-are-type-lambdas-in-scala-and-what-are-their-benefits
 */
object Data_Types_à_La_Carte {

  object `1. Introduction` {

    sealed trait Expr {
      def eval: Int

      def render: String
    }

    case class Val(eval: Int) extends Expr {
      def render = "" + eval
    }

    case class Add(v1: Val, v2: Val) extends Expr {
      def eval: Int = v1.eval + v2.eval

      def render = s"(${v1.render} + ${v2.render})"
    }

  }

  object `2. Fixing_the_expression_problem` {

    sealed trait Expr[f[_]]

    //  case class In[f[Expr[_]]](f0: f[_]) extends Expr[f]

    case class Val[e](i: e)

    //    type IntExpr = Expr[Val[Int]]

    case class Add[e](e1: e, e2: e)

    type AddExpr = Expr[Add]

    sealed trait ⨁[+f[_], +g[_]]

    case class Inl[f[_], g[_], e](f0: f[e]) extends ⨁[f, g]

    case class Inr[f[_], g[_], e](g0: g[e]) extends ⨁[f, g]

    def intOnLeft(i: Int): Val ⨁ Add = Inl(Val(i))

    def intOnRight(i: Int): Val ⨁ Add = Inl(Val(i))

    //    def ia: Nothing⨁Add = Inr(Add(In(intOnLeft(118)), In(intOnRight(1219))))
    //  def addExample(): Expr[⨁] = In(ia)
  }

  object `3. Evaluation` {

    trait Functor[F[_]] {
      def fmap[X, Y](f: X => Y): F[X] => F[Y]
    }

    sealed trait Expr[f[_]]

    //    case class In[f[Expr[_]]](f0: f) extends Expr[f]

    //    case class Val[V](v: V) extends Functor[Val] {
    //      def fmap[X, Y](f: X=>Y): Val[V] => Val[V] = identity[Val[V]]
    //    }

    //    case class Add[e](e1: e, e2: e) extends Functor[Add] {
    //      override def fmap[X, Y](f: (X) => Y): (Add[X]) => Add[Y] = Add(f(e1), f(e2))
    //    }

  }

}
