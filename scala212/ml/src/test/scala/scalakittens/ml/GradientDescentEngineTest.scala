package scalakittens.ml

import org.specs2.mutable.Specification

import scala.language.reflectiveCalls
import scala.math._
import scalakittens.la.Norm
import scalakittens.la.Spaces.R2
import scalakittens.ml.GradientDescentEngine.{DoubleVal, DoubleVar}

/**
  * Created by vpatryshev on 7/13/17.
  */
class GradientDescentEngineTest extends Specification {

  val waves = (vec: R2.Vector) => {
    val x = vec.apply(0)
    val y = vec.apply(1)
    val f = sin(x) + 0.01 * sin(x * 100) + sin(y) + 0.01 * sin(y) + 3
    f
  }

  val wavesGradient = (vec: R2.Vector) => {
    val g0 = new R2.OnFunction(i => {
      val x = vec.apply(i)
      cos(x) + cos(100 * x)
    })
    val g = g0 / Norm.l2(g0)
    g.copy
  }

  "GradientDescentEngineTest" should {
    "find in 1d" in {
      val evaluator = GradientDescentEngine.numericEvaluator(x => 1 + sin(x), x => signum(cos(x)))
      val sut = GradientDescentEngine[DoubleVal, GradientDescentEngine.DoubleVar](evaluator, 400, 0.0001, 0.001)
      val position = DoubleVar(0.0)
      val found = sut.find(position, 1.1)
      found match {
        case Some((err, epoch)) =>
          abs(err) < 0.15 must beTrue
          abs(position() - -1.57) < 0.02 aka s"We are at $position, err=$err, epoch=$epoch" must beTrue
          epoch < 20 aka s"$epoch epochs" must beTrue
        case _ => failure("nothing found")
      }

      ok
    }

    "find in 2d starting at 0" in {
      val evaluator = GradientDescentEngine.vectorEvaluator[R2.type](R2)(waves, wavesGradient)
      val sut = GradientDescentEngine[R2.Vector, R2.MutableVector](evaluator, 30, 0.0001, 0.001)
      val position = R2.Zero.copy
      val found = sut.find(position, 1.1)
      found match {
        case Some((err, epoch)) =>
          math.abs(err - 0.98) < 0.01 aka s"oops, err=$err" must beTrue

          Norm.l2.distance(position, R2.Vector(-1.649, -1.649)) < 0.001 aka s"We are at $position, err=$err, epoch=$epoch" must beTrue
          epoch < 60 aka s"epochs: $epoch" must beTrue
        case _ => failure("nothing found")
      }

      ok
    }

    "find in 2d starting at 1" in {
      val evaluator = GradientDescentEngine.vectorEvaluator[R2.type](R2)(waves, wavesGradient)
      val sut = GradientDescentEngine[R2.Vector, R2.MutableVector](evaluator, 100, 0.0001, 0.001)
      val position = R2.const(1.0).copy
      val found = sut.find(position, 1.1)
      found match {
        case Some((err, epoch)) =>
          math.abs(err - 0.98) < 0.01 aka s"oops, err=$err" must beTrue

          Norm.l2.distance(position, R2.Vector(-1.6, -1.6)) < 0.05 aka s"We are at $position, err=$err, epoch=$epoch" must beTrue
          epoch < 50 aka s"epochs: $epoch" must beTrue
        case _ => failure("nothing found")
      }
      ok
    }

    "find in 2d starting at (1,2)" in {
      val evaluator = GradientDescentEngine.vectorEvaluator[R2.type](R2)(waves, wavesGradient)
      val sut = GradientDescentEngine[R2.Vector, R2.MutableVector](evaluator, 100, 0.0001, 0.001)
      val position = R2.Vector(1, 2).copy
      val found = sut.find(position, 1.1)
      found match {
        case Some((err, epoch)) =>
          math.abs(err - 0.98) < 0.03 aka s"oops, err=$err" must beTrue

          Norm.l2.distance(position, R2.Vector(-1.4,4.8)) < 0.05 aka s"We are at $position, err=$err, epoch=$epoch" must beTrue
          epoch < 50 aka s"epochs: $epoch" must beTrue
        case _ => failure("nothing found")
      }
      ok
    }

    "findAlongGradient 1d starting at 0" in {
      val evaluator = GradientDescentEngine.numericEvaluator(x => 1 + math.sin(x), math.cos)
      val sut = GradientDescentEngine[DoubleVal, GradientDescentEngine.DoubleVar](evaluator, 100, 0.0001, 0.001)
      val position = DoubleVar(0.0)
      val initError = evaluator.error(position)
      val found = sut.findAlongGradient(position, evaluator.gradientAt(position), sut.State(initError, 1.1))
      math.abs(found.error) < 0.15 must beTrue
      math.abs(position() - -1.234) < 0.001 aka s"@$position" must beTrue
      found.epoch < 5 must beTrue
    }

    "findAlongGradient 2d starting at 0" in {
      val evaluator = GradientDescentEngine.vectorEvaluator[R2.type](R2)(waves, wavesGradient)
      val sut = GradientDescentEngine[R2.Vector, R2.MutableVector](evaluator, 100, 0.0001, 0.001)
      val position = R2.Zero.copy
      val initError = evaluator.error(position)
      val found = sut.findAlongGradient(position, evaluator.gradientAt(position), sut.State(initError, 1.1))
      val actualError = evaluator.error(position)
      math.abs(actualError - 0.98) < 0.05 aka s"actually, $actualError" must beTrue
      found.epoch < 10 must beTrue
    }

    "findAlongGradient 2d starting at 1" in {
      val evaluator = GradientDescentEngine.vectorEvaluator[R2.type](R2)(waves, wavesGradient)
      val sut = GradientDescentEngine[R2.Vector, R2.MutableVector](evaluator, 100, 0.0001, 0.001)
      val position = R2.const(1.0).copy
      val initError = evaluator.error(position)
      val found = sut.findAlongGradient(position, evaluator.gradientAt(position), sut.State(initError, 0.1))
      val actualError = evaluator.error(position)
      math.abs(actualError - 4.5) < 0.05 aka s"actually, $actualError" must beTrue
      found.epoch < 10 must beTrue
    }
  }
}
