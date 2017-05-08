package scalakittens.experiments.word2vec

import org.specs2.mutable.Specification

import scalakittens.experiments.word2vec.LinAlg.Vec

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
    
    "multiply by another Vec" in {
      val sut = Array(1.5, 2.25) * Array(2.0, -3.0)
      sut must_== -3.75
    }
  }

}
