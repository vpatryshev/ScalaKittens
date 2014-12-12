package scalakittens

import util.control.ControlThrowable

/**
 * a function with name; good for representing steps
 */
case class Function[-From, +To]
(name:  String,
 act:   From      => To,
 react: Throwable => To = (t:Throwable) => {throw t})
  extends (From => To) {

  override def toString() = name

  def apply(x: From) = try { act(x) } catch {
    case ct: ControlThrowable => throw ct // see "Scala in Depth", page 184: "Carefully catching exceptions"
    case t: Throwable => react(t)
  }

  override def andThen[Next](next: To => Next) = Function[From, Next](name, act andThen next, react andThen next)

}

object Functions {
  type predicate[-P] = Function1[P, Boolean]
  type predicate2[-P1, -P2] = Function2[P1, P2, Boolean]

  implicit def nameIt[From, To](f: From => To) = new {
    def named(name: String) = new Function[From, To](name, f)
  }
}