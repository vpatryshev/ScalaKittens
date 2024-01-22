package scalakittens.experiments.compiler

private[compiler] trait C[X] {
  type D = X
  val ds: Set[D]
}

private[compiler] trait F[Y <: C[_], Z <: C[_]] {
  val y: Y
  val z: Z

  def aD(key: y.D): z.D
}

private[compiler] class C0() extends C[Double] {
  val ds = Set(Double.MinValue, Double.MaxValue)
}

private[compiler] object C00 {
  val c0: C0 = new C0()
}

private[compiler] abstract class G[X <: C[_]](val x: X) extends F[X, C0] {
  val sd: Map[x.D, C00.c0.D]
  override val z: C0 = C00.c0

  def aD(xd: x.D): C00.c0.D = sd(xd)
}

private[compiler] object CompilerProblem {
  def main(args: Array[String]) {
    val c = new C[String] {
      val ds = Set("min", "max")
    }
    val sut = new G[C[String]](c) {
      val sd: Map[String, Double] = Map[String, Double]("pi" → Math.PI)
      override val y: C[String] = c
    }
    println(sut.aD("pi"))
    println("hi there")
  }

}
