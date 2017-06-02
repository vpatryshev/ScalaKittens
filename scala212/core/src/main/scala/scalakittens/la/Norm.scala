package scalakittens.la

import language.postfixOps
import scala.math._

/**
  * all kind of norms; will elaborate further on
  * 
  * Created by vpatryshev on 5/22/17.
  */
trait Norm {
  def apply(xs: Iterable[Double]): Double
  
  def apply(m: Matrix): Double = apply(m.allElements)

  /**
    * converts a vector into a vector of norm=1 (if possible)
    *
    * @return v / norm(v)
    */
  def normalize(v: Vector) = {
    val norm = apply(v)
    if (norm > Double.MinPositiveValue) v / norm else v
  }

  /**
    * Project vector b to vector a
    * @param a vector to project to
    * @param b vector to project
    * @return a projection of b to a
    */
  def project(a: Vector, b: Vector) = a * ((a * b) / apply(a))

  def buildOrthonormalBasis(v: Vector): Array[Vector] = {
    val (maxValue, whereMax) = v.zipWithIndex map {case (x, i) => (abs(x), i)} max

    val vs = new Array[Vector](v.length)

    vs(0) = normalize(v.copy)

    for {
      i <- 1 until v.length
    } {
      val v1: MutableVector = Vector.unit(v.length, if (i < whereMax) i-1 else i).copy
      for (j <- 0 until i) {
        v1 -= project(vs(j), v1)
      }
      vs(i) = normalize(v1)
    }

    vs
  }
}

object Norm {

  /**
    * l<sup>1</sup> norm 
    *
    * sum of absolute values of vector elements
    */
  val l1 = new Norm {
    override def apply(xs: Iterable[Double]): Double = xs map abs sum
  }

  /**
    * l<sup>2</sup> norm 
    *
    * square root of sum of squares of vector elements
    */
  val l2 = new Norm {
    override def apply(xs: Iterable[Double]): Double = sqrt(xs map (x => x*x) sum)

  }

  /**
    * l<sup>∞</sup> norm
    *
    * @return max abs value of elements
    */
  val linf = new Norm {
    override def apply(xs: Iterable[Double]): Double = if (xs.isEmpty) 0.0 else (xs map (x => abs(x))) max
  }
}
