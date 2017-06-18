package scalakittens.la

import org.specs2.mutable.Specification

import Norm._

/**
  * Created by vpatryshev on 5/7/17.
  */
class NormTest extends Specification {
  import math._

  import Spaces._
  
  "Norm" should {

    "normalize" in {
      R0.Vector().normalize(l2) must_== R0.Vector()
      R1.Vector(0).normalize(l2) must_== R1.Vector(0)
      R3.Vector(0, 0, 0).normalize(l2) must_== R3.Vector(0, 0, 0)
      R2.Vector(-sqrt(2), sqrt(2)).normalize(l2) must_== R2.Vector(-sqrt(0.5), sqrt(0.5))
    }
  }
}
