package scalakittens.la

/**
  * Affine transform for vectors
  */
class AffineTransform(val domainSpace: VectorSpace, val codomainSpace: VectorSpace) {
  def apply(matrix: Matrix[domainSpace.Vector, codomainSpace.Vector], shift: domainSpace.Vector) = new (domainSpace.Vector => codomainSpace.Vector) {
    
    /**
      * applies this transform to a vector
      *
      * @param v vector
      * @return the result of transformation
      */
    def apply(v: domainSpace.Vector): codomainSpace.Vector = {
      val shifted: domainSpace.Vector = v - shift
      matrix * shifted
    }
    
    override def toString = s"AffineTransform($shift,\n$matrix)"
  }
}
