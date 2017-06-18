package scalakittens.la

import org.specs2.mutable.Specification

/**
  * Created by vpatryshev on 5/7/17.
  */
class BasisTest extends Specification {
  import math._
  import Spaces._
  
  "Basis" should {

    val sampleUnitaryMatrix_3x3: R3.UnitaryMatrix = {
      val alpha = Pi / 4
      val beta = Pi / 3

      R3.Unitary(
        Array(
          R3.Vector(cos(alpha) * cos(beta), cos(alpha) * sin(beta), sin(alpha)),
          R3.Vector(-sin(alpha) * cos(beta), -sin(alpha) * sin(beta), cos(alpha)),
          R3.Vector(sin(beta), -cos(beta), 0)
        ))
    }

    "transform forth and back" in {
      val baseVector = R3.Vector(1, 1, 1)

      val v0 = R3.Vector(0, 0, 0)
      val v1: R3.Vector = R3.Vector(-0.8660254037844386, 0.5, -1.414213562373095)

      val sut = new R3.Basis(baseVector, sampleUnitaryMatrix_3x3)

      sut(baseVector) must_== v0
      sut.unapply(v0) must_== baseVector
      sut(v0) must_== v1
      val v2 = sut.unapply(v1)
      Norm.l2(v2 - v0) < 0.000001 aka v2.toString must beTrue
      val v3 = Vector(2.6730326074756157, 0.7411809548974794, 1.3660254037844384)
      sut.unapply(baseVector) must_== v3
    }
  }
}
