package scalakittens.la

/**
  * Orthonormal basis in a linear space.
  * Requires a center point and a rotation matrix
  * to transform from an original basis to this one.
  * 
  * Created by vpatryshev on 5/25/17.
  */
case class Basis(center: Vector, rotation: UnitaryMatrix) extends AffineTransform(center, rotation) {

  /**
    * converts a vector from this basis to the original one
    * 
    * @param v vector in this basis
    * @return the same vector in the old basis
    */
  def unapply(v: Vector): Vector = rotation.transpose * v + center
}
