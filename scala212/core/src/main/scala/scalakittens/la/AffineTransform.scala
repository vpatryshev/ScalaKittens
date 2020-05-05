package scalakittens.la

/**
  * Affine transform for vectors
  */
class AffineTransform[Dom <: VectorSpace, Codom <: VectorSpace](
    val domain: Dom, val codomain: Codom) {
  
  def apply(matrix: Matrix[Dom, Codom], shift: Dom#Vector = domain.Zero): (Dom#Vector ⇒ Codom#Vector) = new (Dom#Vector ⇒ Codom#Vector) {
    
    /**
      * applies this transform to a vector
      *
      * @param v vector
      * @return the result of transformation
      */
    def apply(v: Dom#Vector): Codom#Vector = {
//      val vector:Dom#Vector = v - shift // does not compile for some reason
      val shifted = matrix.domain.OnFunction(i ⇒ v(i) - shift(i))
      matrix * shifted
    }
    
    override def toString = s"AffineTransform(\n$matrix,\n$shift\n)"
  }
}

object AffineTransform {
  def apply[Dom <: VectorSpace, Codom <: VectorSpace](domainSpace: Dom, codomainSpace: Codom) = new AffineTransform(domainSpace, codomainSpace)
}
