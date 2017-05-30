package scalakittens.stats

import scalakittens.la.{MutableMatrix, Matrix, Vector}

/**
  * Created by vpatryshev on 5/24/17.
  */
case class AccumulatingMoments(private val size: Int) {
  private var _n: Int = 0
  def n = _n
  val sum: Vector = Vector.Zero(size)()
  private val matrix: MutableMatrix = Matrix(size, size)
  
  def +=(row: Vector): Unit = {

    require(row.length == size, s"All vectors should have the length $size, got ${row.length}")
    _n += 1
    sum += row

    for {
      i <- 0 until size
      j <- 0 until size
    } matrix(i,j) += row(i)*row(j)
  }
  
  def avg = sum / n
  
  def covariance: Matrix = {
    require (_n > 1, s"Can't produce covariance matrix for $n vector(s)")

    new Matrix.OnFunction(size, size,
          (i, j) => (matrix(i, j) - sum(i) * sum(j) / n) / (n-1)
      )
  }
  /**
    * Calculates average of a sequence of vectors
    *
    * @param vectors those to use in calculation
    * @return average
    */
  def apply(vectors: Iterable[Vector]): Vector = {
    vectors foreach +=
    sum/n
    
  }

  def collect(vectors: Iterable[Vector]): AccumulatingMoments = {
    vectors foreach += 
    this
  }
}
