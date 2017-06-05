package scalakittens.la

import scalakittens.la.Vector.{inf, sup}

/**
  * Affine transform for vectors
  * 
  * Created by vpatryshev on 6/4/17.
  */
class AffineTransform(shift: Vector, matrix: Matrix) extends (Vector => Vector) {
  require(matrix.nCols == shift.length, s"Wrong dimensionality, shift is ${shift.length}, matrix is  ${matrix.nRows}x${matrix.nCols}")

  /**
    * applies this transform to a vector
    * 
    * @param v vector
    * @return the result of transformation
    */
  def apply(v: Vector): Vector = {
    require(v.length == shift.length, s"Wrong dimensionality, need ${shift.length}, got ${v.length}")
    matrix * (v - shift)
  }
}

object AffineTransform {
  /**
    * Creates a new affine transform from shift vector and transform matrix
    * @param shift the shift vector
    * @param matrix the matrix of transformation
    * @return a new AffineTransform
    */
  def apply(shift: Vector, matrix: Matrix) = new AffineTransform(shift, matrix)

  /**
    * An affine transform that would map a given sequence of vectors into a unit cube
    * @param dim dimensionality of vectors (there may be zero of them)
    * @param vectors the vectors to transform
    * @return an AffineTransform that would map all these vectors to a unit cube
    */
  def unitCube(dim: Int, vectors: Iterable[Vector]): AffineTransform = {
    val lowerLeft = inf(dim, vectors)
    val upperRight = sup(dim, vectors)
    AffineTransform(lowerLeft, Matrix.diagonal(upperRight - lowerLeft))
  }

  /**
    * Transforms all given vectors into unit cube
    * @param dim vectors length (aka dimensionality)
    * @param vectors the vectors
    * @return an iterable of vectors inside unit cube
    */
  def toUnitCube(dim: Int, vectors: Iterable[Vector]): Iterable[Vector] = {
    val trans = unitCube(dim, vectors)
    vectors map (trans(_))
  }
}
