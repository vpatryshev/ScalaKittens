package scalakittens.la

class Arrays[@specialized(Double) R](implicit ev: R =:= Double) {
  def nudge(xs: Array[R], ys: Array[R], q: Double): Unit = {
    for { i <- xs.indices } {
      xs(i) = (xs(i) + ys(i) * q).asInstanceOf[R]
    }
  }

  def addTo(xs: Array[R], ys: Array[R]): Unit = {
    for { i <- xs.indices } {
      xs(i) = (xs(i) + ys(i)).asInstanceOf[R]
    }
  }

  def subtractFrom(xs: Array[R], ys: Array[R]): Unit = {
    for { i <- xs.indices } {
      xs(i) = (xs(i) - ys(i)).asInstanceOf[R]
    }
  }

  def multBy(xs: Array[R], q: Double): Unit = {
    for { i <- xs.indices } {
      xs(i) = (xs(i) * q).asInstanceOf[R]
    }
  }

  abstract class Folding2 {
    def op(x: R, y: R): Double

    def fold(first: Array[R], second: Array[R]): Double = {
      var s = 0.0
      for { i <- first.indices } s += op(first(i), second(i))
      s
    }

    def foldUpTo(first: Array[R], second: Array[R], limit: Double): Double = {
      var s = 0.0
      for { i <- first.indices } {
        s += op(first(i), second(i))
        if (s >= limit) return s
      }
      s
    }
  }

  private val product = new Folding2 {
    override def op(x: R, y: R): Double = x * y
  }

  def scalarProduct(xs: Array[R], ys: Array[R]): Double = product.fold(xs, ys)

  private val l2distance = new Folding2 {
    override def op(x: R, y: R): Double = {
      val d = x - y
      d * d
    }
  }

  def l2(xs: Array[R]): Double = Math.sqrt(scalarProduct(xs, xs))

  def l2(xs: Array[R], ys: Array[R]): Double = {
    val squareDistance = l2distance.fold(xs, ys)
    Math.sqrt(squareDistance)
  }

  val sammonErrorMeasure: Folding2 = new Folding2() {
    override def op(x: R, y: R): Double = {
      val d = x - y
      if (y == 0.0) 0.0
      else d * d / y // zeroes are on diagonal

    }
  }
}

object DoubleArrays extends Arrays[Double]