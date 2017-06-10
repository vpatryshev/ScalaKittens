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
