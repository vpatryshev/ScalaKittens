package scalakittens.stats

import scalakittens.la._

/**
  * Created by vpatryshev on 5/24/17.
  */
class AccumulatingMoments[Space <: VectorSpace](val space: Space) {
  private var _n: Int = 0
  def n = _n
  val sum: space.MutableVector = space.Zero.copy
  private val matrix: MutableMatrix[Space, Space] = Matrix(space, space)
  
  def +=(row: space.Vector): Unit = {
    _n += 1
    sum += row

    for {
      i <- 0 until space.dim
      j <- 0 until space.dim
    } matrix(i,j) += row(i)*row(j)
  }
  
  def avg: Space#MutableVector = (sum / n) .copy 
  
  // TODO: get rid of casting
  def covariance[S <: Space]: S#SquareMatrix = {
    require (_n > 1, s"Can't produce covariance matrix for $n vector(s)")
    space.squareMatrix((i, j) => (matrix(i, j) - sum(i) * sum(j) / n) / (n-1)) .asInstanceOf[S#SquareMatrix]
  }
  
  /**
    * Collects moments of a sequence of vectors
    *
    * @param vectors those to use in calculation
    * @return this
    *         
    * TODO: make it type-safe
    */
  def collect[V <: Space#Vector](vectors: TraversableOnce[V]): AccumulatingMoments[Space] = {
    vectors foreach ((v:V) => this.+=(v.asInstanceOf[space.Vector])) 
    this
  }
}
