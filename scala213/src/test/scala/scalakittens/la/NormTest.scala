package scalakittens.la

import org.specs2.mutable.Specification
import math._

/**
  * Some normed spaces being tested
  * Created by vpatryshev on 11/30/17.
  */
class NormTest extends Specification {

  "l1" should {
    "work in 0 dimensions" in {
      Norm.l1(Nil) must_== 0.0
    }
    "work in 1 dimension" in {
      Norm.l1(Pi::Nil) must_== math.Pi
    }
    "work in 3 dimensions" in {
      Norm.l1(Pi::(-Pi)::E::Nil) must_== E + 2*Pi
    }
  }

  "l2" should {
    "work in 0 dimensions" in {
      Norm.l2(Nil) must_== 0.0
    }
    "work in 1 dimension" in {
      Norm.l2(Pi::Nil) must_== math.Pi
    }
    "work in 2 dimensions" in {
      Norm.l2(Pi::(-Pi)::Nil) must_== sqrt(2)*Pi
    }
    "work in 3 dimensions" in {
      Norm.l2(Pi::(-Pi)::E::Nil) must_== sqrt(E*E + 2*Pi*Pi)
    }
  }

  "l∞" should {
    "work in 0 dimensions" in {
      Norm.linf(Nil) must_== 0.0
    }
    "work in 1 dimension" in {
      Norm.linf(Pi::Nil) must_== math.Pi
    }
    "work in 2 dimensions" in {
      Norm.linf(Pi::(-Pi)::Nil) must_== Pi
    }
    "work in 3 dimensions" in {
      Norm.linf(Pi::(-Pi)::E::Nil) must_== Pi
    }
  }
}
