package scalakittens.stats

import scalakittens.la._

/**
  * Created by vpatryshev on 5/24/17.
  */
case class AccumulatingMoments(val space: VectorSpace) {
  private var _n: Int = 0
  def n = _n
  val sum: space.MutableVector = space.Zero.copy
  private val matrix: MutableMatrix[space.type, space.type] = Matrix(space, space)
  
  def +=(row: space.Vector): Unit = {
    _n += 1
    sum += row

    for {
      i <- 0 until space.dim
      j <- 0 until space.dim
    } matrix(i,j) += row(i)*row(j)
  }
  
  def avg = (sum / n) .copy  
  
  def covariance: Matrix[space.type, space.type] = {
    require (_n > 1, s"Can't produce covariance matrix for $n vector(s)")

    new Matrix.OnFunction(space, space,
          (i, j) => (matrix(i, j) - sum(i) * sum(j) / n) / (n-1)
      )
  }
  /**
    * Calculates average of a sequence of vectors
    *
    * @param vectors those to use in calculation
    * @return average
    */
  def apply(vectors: Iterable[space.Vector]): Unit = {
    vectors foreach +=
  }

  def collect(vectors: Iterable[space.Vector]): AccumulatingMoments = {
    vectors foreach += 
    this
  }
}
