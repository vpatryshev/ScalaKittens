package scalakittens.experiments

object ChurchEncoding {

  val True: Any => Any => Any = (x1: Any) => (x2: Any) => x1
  val False: Any => Any => Any = (x1: Any) => (x2: Any) => x2
  val p1: pair[Int, String] = pair(1, "abc")
  val j42: Maybe[Int] = just(42)

  def ⊥⊥ : Nothing = ⊥(⊥)

  p1(x => y => s"($x, $y)")

  def ⊥[X]: X => Nothing = (_: X) => throw new RuntimeException("⊥")

  def nothing[X]: Maybe[X] = new Maybe[X] {
    override def apply[Y](l: Nothing => Y, r: X => Y) = l(⊥⊥)

    override def apply[Y](n: => Y, j: X => Y): Y = n
  }

  def just[X](x: X): Maybe[X] = new Maybe[X] {
    override def apply[Y](l: Nothing => Y, r: X => Y): Y = r(x)

    override def apply[Y](n: => Y, j: X => Y): Y = j(x)
  }

  trait Union[+A, +B] {
    def apply[C](l: A => C, r: B => C): C
  }

  trait Maybe[+X] extends Union[Nothing, X] {
    def apply[Y](n: => Y = ⊥⊥, j: X => Y): Y
  }

  case class pair[+X, +Y](x: X, y: Y) {
    def apply[Z](f: X => Y => Z): Z = f(x)(y)
  }

//  nothing(n = False)("x")("y")

  object taktak

  j42("question", 3 + _)

  //def isNothing(m: Maybe[_]) = m True
  /* js:
  var T = x => y => x
  var F = x => y => y
  var pair = x => y => f => f(x)(y)
  var union = f => g => u => u(f)(g)
  var left = x => f => g => f(x)
  var right = y => f => g => g(y)
  
   */


}
