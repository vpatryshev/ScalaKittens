package scalakittens.ml

import org.specs2.mutable.Specification

import scalakittens.la.Spaces.R2
import math.sin
import math.cos

/**
  * Created by vpatryshev on 7/13/17.
  */
class GradientDescentEngineTest extends Specification {

  val waves = (vec:R2.Vector) => {
    val x = vec.apply(0)
    val y = vec.apply(1)
    sin(x) + 0.01*sin(x*100) + sin(y) + 0.01*sin(y) + 3
  }

  val wavesGradient = (vec:R2.Vector) => {
    val x = vec.apply(0)
    val y = vec.apply(1)
    new R2.OnFunction(i => {
      val x = vec.apply(i)
      cos(x) + cos(100*x)
    }).copy
  }

  "GradientDescentEngineTest" should {
    "find" in {
      ok
    }

    "findAlongGradient" in {
      val evaluator = GradientDescentEngine.vectorEvaluator[R2.type](R2)(waves, wavesGradient)
      val sut = GradientDescentEngine[R2.Vector, R2.MutableVector](evaluator, 100, 0.0001)
      val initPos = R2.Zero.copy
      val initError = evaluator.error(initPos)
      val found = sut.findAlongGradient(initPos, evaluator.gradientAt(initPos), sut.State(initError, 0.1))
      found.error must_== initError
      found.epoch < 30 must beTrue
    }
  }
}
