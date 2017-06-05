package scalakittens.la

import org.specs2.mutable.Specification

/**
  * Created by vpatryshev on 5/7/17.
  */
class AffineTransformTest extends Specification {
  
  "AffineTransform" should {

    "transform" in {
      val m0: Matrix = Matrix(0, 3, Vector())
      val sut0 = new AffineTransform(Vector(1,1,1), m0)
      sut0(Vector(0,0,0)) must_== Vector()

      new AffineTransform(Vector(1,2,3), Matrix.Unit(3))(Vector(0,0,0)) must_== Vector(-1,-2,-3)

      val sut1 = new AffineTransform(Vector(1,1,1), Matrix.ofRows(3, Array(Vector(0, 0, 0), Vector(2,3,4))))
      sut1(Vector(2,3,4)) must_== Vector(0, 20)
      
      val sut2 = new AffineTransform(Vector(1,2), Matrix.ofRows(2, Array(Vector(0,1), Vector(1,0), Vector(1,-1))))
      sut2(Vector(1,1)) must_== Vector(-1, 0, 1)
    }
  }
}
