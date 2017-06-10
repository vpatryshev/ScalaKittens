package scalakittens.la

/**
  * Affine transform for vectors
  */
class AffineTransform(val matrix: Matrix){
  def apply(shift: matrix.domain.Vector) = new (matrix.domain.Vector => matrix.codomain.Vector) {
    
    /**
      * applies this transform to a vector
      *
      * @param v vector
      * @return the result of transformation
      */
    def apply(v: matrix.domain.Vector): matrix.codomain.Vector = {
      val shifted = v - shift
      matrix * shifted
    }
    override def toString = s"AffineTransform($shift,\n$matrix)"
  }
}
