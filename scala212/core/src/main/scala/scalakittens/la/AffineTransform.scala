package scalakittens.la

/**
  * Affine transform for vectors
  */
class AffineTransform[Dom <: VectorSpace, Codom <: VectorSpace](val domainSpace: Dom, val codomainSpace: Codom) {
  
  def apply(matrix: Matrix[Dom, Codom], shift: Dom#Vector = domainSpace.Zero) = new (Dom#Vector => Codom#Vector) {
    
    /**
      * applies this transform to a vector
      *
      * @param v vector
      * @return the result of transformation
      */
    def apply(v: Dom#Vector): Codom#Vector = {
//      val vector = v - shift - did not compile for some reason
      val shifted: Dom#Vector = new domainSpace.OnFunction(i => v(i) - shift(i))
      matrix * shifted
    }
    
    override def toString = s"AffineTransform(\n$matrix,\n$shift\n)"
  }
}

object AffineTransform {
  def apply[Dom <: VectorSpace, Codom <: VectorSpace](domainSpace: Dom, codomainSpace: Codom) = new AffineTransform(domainSpace, codomainSpace)
}
