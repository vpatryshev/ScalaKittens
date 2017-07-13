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
  
  def apply(m: Matrix[_,_]): Double = apply(m.allElements)
  
  def distance(xs: IndexedSeq[Double], ys: IndexedSeq[Double]) = {
    this(xs.indices map { i => xs(i) - ys(i) } view)
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
    override def apply(xs: Iterable[Double]): Double = {
      xs match {
        case ax: VectorSpace#OnArray =>
          ArrayOps.l2(ax.data)
        case _ =>
          sqrt(xs map (x => x*x) sum)
      }
    }

    override def distance(xs: IndexedSeq[Double], ys: IndexedSeq[Double]): Double = {
      (xs, ys) match {
        case (ax: VectorSpace#OnArray, ay: VectorSpace#OnArray) =>
          ArrayOps.l2(ax.data, ay.data)
        case _ => super.distance(xs, ys)
      }

    }

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
