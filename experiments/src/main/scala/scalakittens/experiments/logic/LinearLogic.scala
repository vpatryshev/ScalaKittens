package scalakittens.experiments.logic

import scala.language.implicitConversions

/**
 * My experimental stuff.
 * Checking if linear logic can be reasonably implemented in Scala
 */
object LinearLogic {
  type &[+T, +U] = (T, U)
  type |[+T, +U] = Either[T, U]

  trait ConjunctionDisjunction[T] {
    def &[U](u: U): &[T, U]

    def |[U](u: U): |[T, U]
  }

  implicit def lowLevel[T](t: T): ConjunctionDisjunction[T] = new ConjunctionDisjunction[T] {
    def &[U](u: U): &[T, U] = (t, u)

    def |[U](u: U): |[T, U] = ??? // I just don't know how to implement it
  }

  object ⊤

  object ⊥

  val _1_ : Unit = ()
  val _0_ : Nothing = ??? // this may be the right implementation

  // The exchange rule: If a sequent is valid, then any permutation of it (created by permuting its left and right 
  // sides independently) is valid.
  def swap[X, Y]: (X, Y) => (Y, X) = (x: X, y: Y) ⇒ (y, x)

  //The restricted weakening rule: If Γ,Δ⊢Θ, then Γ,!A,Δ⊢Θ, for any A; conversely and dually, if Γ⊢Δ,Θ, then Γ⊢Δ,?A,Θ
  // for any A.


  trait Promise[+T] {
    def ¬ : Promise[T]

    def ⊗[U](uOpt: Promise[U]): Promise[T & U]

    def ⊕[U](uOpt: Promise[U]): Promise[T | U]

    def ! : T
  }

  case class Just[T](t: T) extends Promise[T] {
    override def ¬ : Promise[T] = None

    override def ⊕[U](uOpt: Promise[U]): Promise[T | U] = uOpt match {
      case Just(u) ⇒ Just(t | u)
      case None ⇒ None // ? you sure?
    }

    override def ! : T = t

    override def ⊗[U](uOpt: Promise[U]): Promise[(T, U)] = uOpt match {
      case Just(u) ⇒ Just(t & u)
      case None ⇒ None
    }
  }

  def ?[T](t: T): Just[T] = Just(t)


  def diagonal[T]: T ⇒ T & T = t ⇒ (t, t) // restricted contraction rule
  // def ... what?  ?(x) & ?(x) ⇒ ?(x) // dual to restricted contraction rule
  //??? If Γ⊢A,Δ, then Γ,A⊥⊢Δ; conversely and dually, if Γ,A⊢Δ, then Γ⊢A⊥,Δ.

  // Either[X⇒A, X⇒B] ⇒ X ⇒ A ⊕ B ????
  // Either[A ⇒ X, B ⇒ X] ⇒ A&B ⇒ X // same with ⊗

  //  ((f:X ⇒ A),(g:X ⇒ B) ⇒ (f,g):X ⇒ A&B

  // (A⇒X,B⇒X) ⇒ (A⊕B ⇒ X)

  case object None extends Promise[Nothing] {
    override def ¬ : Promise[Nothing] = Just(_0_)

    override def ⊕[U](uOpt: Promise[U]): Promise[Nothing | U] = ???

    override def ! : Nothing = ???

    override def ⊗[U](uOpt: Promise[U]): Promise[Nothing & U] = ??? //None
  }

}
