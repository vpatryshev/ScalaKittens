package scalakittens.applicative

trait Monoid[O] {
  val _0: O
  def add(x: O, y: O): O

  case class Acc[A](value: O) {
    def <+>(another: O): O = add(value, another)
  }

  implicit def acc[A](x: O): Acc[A] = Acc(x)

  object App extends ConstantFunctor[Acc] with Applicative[Acc] {

    override def pure[A](a: A) = acc[A](_0)

    override implicit def applicable[A, B](ff: Acc[A => B]) = new Applicable[A, B] {
      def <*>(fa: Acc[A]) = Acc(ff <+> fa.value)
    }
  }

  def accumulate[A,T[_]](traversable: Traversable[T])(eval: A => O)(ta: T[A]): O = {
    val evalToAcc: (A) => Acc[O] = eval andThen acc
    traversable.traverse(App)(evalToAcc)(ta).value
  }
}
