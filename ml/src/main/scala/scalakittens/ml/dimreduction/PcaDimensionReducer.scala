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
    val moments = new AccumulatingMoments[Source](source)
    originalVectors foreach(v => moments.add(v.asInstanceOf[moments.space.Vector]))
    // TODO: get rid of casting
    moments.collect(originalVectors map (_.asInstanceOf[moments.space.Vector]))
    val avg = moments.avg
    val cov: Source#SquareMatrix = moments.covariance
    val iterations = PCA.Iterations(precision, numIterations)
    val buildEigenVectors = iterations.buildEigenVectors(source) _
    // TODO: get rid of casting
    val eigens = buildEigenVectors(cov.asInstanceOf[source.SquareMatrix], target.dim)

    val vectors = eigens map (_._2.copy)
    val transformBase:AffineTransform[Source, Target] = AffineTransform(source, target)

    val matrix: Matrix[Source, Target] = new source.RowMatrix[Target](target, vectors). asInstanceOf[Matrix[Source, Target]] // this sucks, but well, variance + Curry typing leave me no choice
    val trans = transformBase(matrix, avg)

    val vs = originalVectors map trans
    vs
//IndexedSeq[Target#Vector]
  }
}
