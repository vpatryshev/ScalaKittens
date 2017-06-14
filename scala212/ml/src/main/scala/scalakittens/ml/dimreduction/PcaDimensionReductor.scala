package scalakittens.ml.dimreduction

import scalakittens.la.{AffineTransform, Matrix, PCA, Vector}
import scalakittens.stats.AccumulatingMoments

/**
  * Created by vpatryshev on 6/11/17.
  */
class PcaDimensionReductor(val sourceDim: Int, val targetDim: Int, precision: Double, numIterations: Int) extends DimensionReductor {

  def apply(originalVectors: IndexedSeq[Vector]): IndexedSeq[Vector] = {
    val acc = AccumulatingMoments(sourceDim).collect(originalVectors)
    val avg = acc.avg
    val cov = acc.covariance
    //    println(s"avg=$avg")
    //    println(s"covariance=\n$cov\n\n")
    val Some(eigens) = PCA.Iterations(precision, numIterations).buildEigenVectors(cov, targetDim)
    //    println("\nEIGENVALUES:\n")
    //    println(eigens map (_._1))
    val trans = new AffineTransform(avg, Matrix.ofRows(sourceDim, eigens.map(_._2).toArray))

    val vs = originalVectors map trans
    vs
  }
}
