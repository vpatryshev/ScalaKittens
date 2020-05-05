package scalakittens.ml

import org.specs2.mutable.Specification

import scala.math._
import scalakittens.la.Norm
import scalakittens.la.Spaces.R2
import scalakittens.ml.GradientDescentEngine.{DoubleVal, DoubleVar}

/**
  * Created by vpatryshev on 7/13/17.
  */
class GradientDescentEngineTest extends Specification {
  sequential
  def waves(vec: R2.Vector): Double = {
    val x = vec.apply(0)
    val y = vec.apply(1)
    val f = sin(x) + 0.01 * sin(x * 100) + sin(y) + 0.01 * sin(y) + 3
    f
  }

  def wavesGradient(vec: R2.Vector): R2.Vector = {
    val g0 = R2.OnFunction(i ⇒ {
      val x = vec.apply(i)
      cos(x) + cos(100 * x)
    })
    val g = g0 / Norm.l2(g0)
    g.copy
  }

  "GradientDescentEngineTest" should {

    "find in 2d starting at (1,2)" in {
      val evaluator = GradientDescentEngine.vectorEvaluator[R2.type](R2)(waves, wavesGradient)
      val sut = GradientDescentEngine[R2.MutableVector, R2.Vector](evaluator, 100, 0.0001, 0.001)
      val position = R2.Vector(1, 2).copy
      val found = sut.find(position, 1.1)
      found match {
        case Some((value, epoch)) ⇒
          math.abs(value - 0.98) < 0.03 aka s"oops, value=$value" must beTrue

          Norm.l2.distance(position, R2.Vector(-1.4,4.8)) < 0.05 aka s"We are at $position, value=$value, epoch=$epoch" must beTrue
          epoch < 50 aka s"epochs: $epoch" must beTrue
        case _ ⇒ failure("nothing found")
      }
      ok
    }

    "find in 1d" in {
      val evaluator = GradientDescentEngine.numericEvaluator(x ⇒ 1 + sin(x), x ⇒ signum(cos(x)))
      val sut = GradientDescentEngine[DoubleVar, DoubleVal](evaluator, 100, 0.0001, 0.001)
      val position = DoubleVar(0.0)
      val found = sut.find(position, 1.1)
      found match {
        case Some((value, epoch)) ⇒
          abs(value) < 0.15 must beTrue
          abs(position() - -1.58) < 0.02 aka s"We are at $position, value=$value, epoch=$epoch" must beTrue
          epoch < 50 aka s"$epoch epochs" must beTrue
        case _ ⇒ failure("nothing found")
      }

      ok
    }

    "find in 2d starting at 0" in {
      val evaluator = GradientDescentEngine.vectorEvaluator[R2.type](R2)(waves, wavesGradient)
      val sut = GradientDescentEngine[R2.MutableVector, R2.Vector](evaluator, 50, 0.0001, 0.001)
      val position = R2.Zero.copy
      val found = sut.find(position, 1.1)
      found match {
        case Some((value, epoch)) ⇒
          math.abs(value - 0.98) < 0.01 aka s"oops, value=$value" must beTrue

          Norm.l2.distance(position, R2.Vector(-1.649, -1.649)) < 0.005 aka s"We are at $position, value=$value, epoch=$epoch" must beTrue
          epoch < 60 aka s"epochs: $epoch" must beTrue
        case _ ⇒ failure("nothing found")
      }

      ok
    }

    "find in 2d starting at 1" in {
      val evaluator = GradientDescentEngine.vectorEvaluator[R2.type](R2)(waves, wavesGradient)
      val sut = GradientDescentEngine[R2.MutableVector, R2.Vector](evaluator, 200, 0.0001, 0.001)
      val position = R2.const(1.0).copy
      val found = sut.find(position, 1.1)
      found match {
        case Some((value, epoch)) ⇒
          math.abs(value - 0.98) < 0.05 aka s"oops, value=$value" must beTrue

          Norm.l2.distance(position, R2.Vector(-1.6, -1.6)) < 0.2 aka s"We are at $position, value=$value, epoch=$epoch" must beTrue
          epoch < 50 aka s"epochs: $epoch" must beTrue
        case _ ⇒ failure("nothing found")
      }
      ok
    }

    "findAlongGradient 1d starting at 0" in {
      val evaluator = GradientDescentEngine.numericEvaluator(x ⇒ 1 + math.sin(x), math.cos)
      val sut = GradientDescentEngine[DoubleVar, DoubleVal](evaluator, 100, 0.0001, 0.001)
      val position = DoubleVar(0.0)
      val initValue = evaluator.targetFunction(position)
      val found = sut.findAlongGradient(position, evaluator.gradientAt(position), sut.Cursor(initValue, 1.1))
      math.abs(found.functionValue) < 0.15 must beTrue
      math.abs(position() - -1.234) < 0.001 aka s"@$position" must beTrue
      found.counter < 5 must beTrue
    }

    "findAlongGradient 2d starting at 0" in {
      val evaluator = GradientDescentEngine.vectorEvaluator[R2.type](R2)(waves, wavesGradient)
      val sut = GradientDescentEngine[R2.MutableVector, R2.Vector](evaluator, 100, 0.0001, 0.001)
      val position = R2.Zero.copy
      val initValue = evaluator.targetFunction(position)
      val found = sut.findAlongGradient(position, evaluator.gradientAt(position), sut.Cursor(initValue, 1.1))
      val actualValue = evaluator.targetFunction(position)
      math.abs(actualValue - 0.98) < 0.05 aka s"actually, $actualValue" must beTrue
      found.counter < 10 must beTrue
    }

    "findAlongGradient 2d starting at 1" in {
      val evaluator = GradientDescentEngine.vectorEvaluator[R2.type](R2)(waves, wavesGradient)
      val sut = GradientDescentEngine[R2.MutableVector, R2.Vector](evaluator, 100, 0.0001, 0.001)
      val position = R2.const(1.0).copy
      val initValue = evaluator.targetFunction(position)
      val found = sut.findAlongGradient(position, evaluator.gradientAt(position), sut.Cursor(initValue, 0.1))
      val actualValue = evaluator.targetFunction(position)
      math.abs(actualValue - 4.5) < 0.05 aka s"actually, $actualValue" must beTrue
      found.counter < 10 must beTrue
    }
  }
}
