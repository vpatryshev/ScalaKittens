package scalakittens.ml.dimreduction

import scalakittens.la._
import scalakittens.stats.AccumulatingMoments

/**
  * Created by vpatryshev on 6/11/17.
  */
class PcaDimensionReducer[Source <: VectorSpace, Target <: VectorSpace](
  val source: Source,
  val target: Target, 
  precision: Double, 
  numIterations: Int) 
  extends DimensionReducer[Source, Target] {

  def reduce(originalVectors: IndexedSeq[Source#Vector]): IndexedSeq[Target#Vector] = {
    val acc = new AccumulatingMoments[Source](source).collect(originalVectors)
    val avg = acc.avg
    val cov = acc.covariance
    //    println(s"avg=$avg")
    //    println(s"covariance=\n$cov\n\n")
    val eigens = PCA.Iterations(precision, numIterations).buildEigenVectors(source)(cov, target.dim)
//        println("\nEIGENVALUES:\n")
//        println(eigens map (_._1))
    val vectors = eigens map (_._2.copy)
    val transformBase:AffineTransform[Source, Target] = AffineTransform(source, target)

    val matrix: Matrix[Source, Target] = new source.RowMatrix[Target](target, vectors). asInstanceOf[Matrix[Source, Target]] // this sucks, but well, variance + Curry typing leave me no choice
    val trans = transformBase(matrix, avg)

    val vs = originalVectors map trans
    vs
//IndexedSeq[Target#Vector]
  }
}
