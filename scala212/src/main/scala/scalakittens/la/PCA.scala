package scalakittens.la

import language.postfixOps
import scala.math._

/**
  * Created by vpatryshev on 5/17/17.
  */
object PCA {
  case class Iterations(precision: Double, maxRepeats: Int) {
    
    private def oneStep(m: Matrix, v: Vector): (Vector, Double) = {
      val v1 = m * v normalize
      val d = (v1 - v).l2
      (v1, d)
    }
    
    def eigenValue(m: Matrix): Option[(Double, Vector, Int)] = {

      require(m.nCols == m.nRows, s"Expected a square matrix, have ${m.nRows}⨯${m.nCols}")
      val iterator = Iterator.iterate((Vector.unit(m.nCols, 0), Double.MaxValue, 0)) {
        case (v, d, i) => val (m1, d1) = oneStep(m, v); (m1, d1, i + 1)
      }

      val goodData = iterator.dropWhile(p => p._2 > precision && p._3 < maxRepeats) take 1 toList

      goodData.headOption map { case (v, d, i) =>
        ((m * v).sum / v.sum, v, i)
      }
    }
  }
}
