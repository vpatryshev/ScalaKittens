package scalakittens.ml

import org.omg.CORBA.DoubleHolder
import org.specs2.mutable.Specification

import scalakittens.la.Spaces.R2
import math.sin
import math.cos
import scalakittens.ml.GradientDescentEngine.{DoubleVal, DoubleVar}

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

    "findAlongGradient 1d starting at 0" in {
      val evaluator = GradientDescentEngine.numericEvaluator(x => 1+math.sin(x), math.cos)
      val sut = GradientDescentEngine[DoubleVal, GradientDescentEngine.DoubleVar](evaluator, 100, 0.0001)
      val position = DoubleVar(0.0)
      import scalakittens.ml.GradientDescentEngine.DoubleVal
      val initError = evaluator.error(position)
      val found = sut.findAlongGradient(position, evaluator.gradientAt(position), sut.State(initError, 1.1))
      math.abs(found.error) < 0.15 must beTrue
      math.abs(position() - -1.234) < 0.001 must beTrue
      found.epoch < 5 must beTrue
    }
    
    "findAlongGradient 2d starting at 0" in {
      val evaluator = GradientDescentEngine.vectorEvaluator[R2.type](R2)(waves, wavesGradient)
      val sut = GradientDescentEngine[R2.Vector, R2.MutableVector](evaluator, 100, 0.0001)
      val position = R2.Zero.copy
      val initError = evaluator.error(position)
      val found = sut.findAlongGradient(position, evaluator.gradientAt(position), sut.State(initError, 1.1))
      val actualError = evaluator.error(position)
      math.abs(actualError  - 1.4) < 0.05 aka s"actually, $actualError" must beTrue
      found.epoch < 10 must beTrue
    }

    "findAlongGradient 2d starting at 1" in {
      val evaluator = GradientDescentEngine.vectorEvaluator[R2.type](R2)(waves, wavesGradient)
      val sut = GradientDescentEngine[R2.Vector, R2.MutableVector](evaluator, 100, 0.0001)
      val position = R2.const(1.0).copy
      val initError = evaluator.error(position)
      val found = sut.findAlongGradient(position, evaluator.gradientAt(position), sut.State(initError, 0.1))
      val actualError = evaluator.error(position)
      math.abs(actualError  - 2.4) < 0.05 aka s"actually, $actualError" must beTrue
      found.epoch < 10 must beTrue
    }
  }
}
