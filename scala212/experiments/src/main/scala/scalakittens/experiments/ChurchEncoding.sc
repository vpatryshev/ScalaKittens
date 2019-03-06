import scalakittens.experiments.logic.LinearLogic
import scalakittens.experiments.logic.LinearLogic.⊥

val True  = (x1: Any) => (x2: Any) => x1
val False = (x1: Any) => (x2: Any) => x2

case class pair[+X, +Y](x: X, y: Y) {
  def apply[Z](f: X => Y => Z) = f(x)(y)
}

val p1 = pair(1, "abc")

p1(x => y => s"($x, $y)")

trait Union[+A, +B] {
  def apply[C](l: A => C, r: B => C): C
}

def ⊥[X] = (_:X) => throw new RuntimeException("⊥")
def ⊥⊥ = ⊥(⊥)

trait Maybe[+X] extends Union[Nothing, X] {
  def apply[Y](n: => Y = ⊥⊥, j: X => Y = LinearLogic.⊥): Y = apply(l= (_:_) => n, j)
}

def nothing[X]: Maybe[X] = new Maybe[X] {
  override def apply[Y](l: Nothing => Y, r: X => Y) = l(())
}

def just[X](x: X): Maybe[X] = new Maybe[X] {
  override def apply[Y](l: Nothing => Y, r: X => Y): Y = r(x)
}

nothing(n = False)("x")("y")
val j42 = just(42)

j42("question", 3+_)

//def isNothing(m: Maybe[_]) = m True
/* js:
var T = x => y => x
var F = x => y => y
var pair = x => y => f => f(x)(y)
var union = f => g => u => u(f)(g)
var left = x => f => g => f(x)
var right = y => f => g => g(y)

 */