package scalakittens.experiments.word2vec

import org.specs2.mutable.Specification

/**
  * Created by vpatryshev on 5/7/17.
  */
class LinAlgTest extends Specification {
  import LinAlg._

  "Vec" should {
    
    "multiply by scalar" in {
      val sut = Array(1.5, 2.25) * 3.0
      sut.data must_== Array(4.5, 6.75)
    }

    "multiply by scalar in place" in {
      val sut = Array(1.5, 2.25)
      sut *= 3.0
      sut.data must_== Array(4.5, 6.75)
    }

    "multiply by another Vec" in {
      val sut = Array(1.5, 2.25) * Array(2.0, -3.0)
      sut must_== -3.75
    }

    "add another vec" in {
      val sut = Array(3.25, -7.5) + Array(6.0, 4.25)
      sut.data must_== Array(9.25, -3.25)
    }

    "add another vec in place" in {
      val sut = Array(3.25, -7.5)
      sut += Array(6.0, 4.25)
      sut.data must_== Array(9.25, -3.25)
    }

    "subtract another vec" in {
      val sut = Array(3.25, -7.5) - Array(6.0, 4.25)
      sut.data must_== Array(-2.75, -11.75)
    }

    "subtract another vec in place" in {
      val sut = Array(3.25, -7.5)
      sut -= Array(6.0, 4.25)
      sut.data must_== Array(-2.75, -11.75)
    }
  }

}
