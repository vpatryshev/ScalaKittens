package scalakittens.la

import org.specs2.mutable.Specification

/**
  * Created by vpatryshev on 5/7/17.
  */
class AffineTransformTest extends Specification {
  import Spaces._

  "AffineTransform" should {

    "transform" in {
      val m0 = Matrix.build[R3.type, R0.type](R3, R0)
      
      val sut0 = new AffineTransform[R3.type, R0.type](R3, R0).apply(m0, R3.Vector(1,1,1))
      
      sut0(R3.Vector(0,0,0)) must_== R0.Vector()

      val sut1 = new AffineTransform[R3.type, R3.type](R3,R3)
      
      val trans1 = sut1(R3.UnitMatrix,R3.Vector(1,2,3))
      
      trans1(R3.Vector(0,0,0)) must_== R3.Vector(-1,-2,-3)

      val sut2 = new AffineTransform(R3,R2)(Matrix(R3, R2, Array(0, 0, 0, 2,3,4)), R3.Vector(1,1,1))
      
      sut2(R3.Vector(2,3,4)) must_== R2.Vector(0, 20)
      
      val sut3 = new AffineTransform(R2,R3)(Matrix(R2, R3, Array(0,1,1,0,1,-1)), R2.Vector(1,2))
      sut3(R2.Vector(1,1)) must_== R3.Vector(-1, 0, 1)
    }
  }
}
