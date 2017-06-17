package scalakittens.la

/**
  * Affine transform for vectors
  */
class AffineTransform[Dom <: VectorSpace, Codom <: VectorSpace](val domainSpace: Dom, val codomainSpace: Codom) {
  def apply(matrix: Matrix[Dom, Codom], shift: domainSpace.Vector) = new (domainSpace.Vector => Codom#Vector) {
    
    /**
      * applies this transform to a vector
      *
      * @param v vector
      * @return the result of transformation
      */
    def apply(v: domainSpace.Vector): Codom#Vector = {
      val shifted: Dom#Vector = v - shift
      matrix * shifted
    }
    
    override def toString = s"AffineTransform($shift,\n$matrix)"
  }
}
