package scalakittens.la

import scala.math._

/**
  * all kind of norms; will elaborate further on
  * 
  * Created by vpatryshev on 5/22/17.
  */
trait Norm {

  def apply(xs: Iterator[Double]): Double

  def apply(v: Seq[Double]): Double = apply(v.iterator)
  def apply(m: Matrix[_,_]): Double = apply(m.iterator)
  
  def distance(xs: IndexedSeq[Double], ys: IndexedSeq[Double]): Double = {
    this(xs.indices map { i => xs(i) - ys(i) } iterator)
  }
      
}

object Norm {

  /**
    * l<sup>1</sup> norm 
    *
    * sum of absolute values of vector elements
    */
  object l1 extends Norm {
    override def apply(xs: Iterator[Double]): Double = xs map abs sum
  }

  /**
    * l<sup>2</sup> norm 
    *
    * square root of sum of squares of vector elements
    */
  object l2 extends Norm {
//    override def apply(xs: Iterable[Double]): Double = {
//      xs match {
//        case ax: VectorSpace#Vector => ax.l2
//        case _ => sqrt(xs map (x => x*x) sum)
//      }
//    }

    def apply(xs: Iterator[Double]): Double = sqrt(xs map (x => x*x) sum)

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
  object linf extends Norm {
    override def apply(xs: Iterator[Double]): Double = (xs map abs).foldLeft(0.0)(max)
  }
}
