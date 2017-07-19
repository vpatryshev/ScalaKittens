package scalakittens.ml

import com.sun.prism.paint.Gradient

import scala.language.postfixOps
import scalakittens.la.VectorSpace
import scalakittens.ml.GradientDescentEngine.Evaluator

/**
  * Pretty generic tool for gradient descent.
  * It knows nothing about how to calculate target function, or how to calculate gradient.
  * It just does the search.
  * The fact that we are in a linear space is also irrelevant.
  * Created by vpatryshev on 7/13/17.
  */
sealed case class GradientDescentEngine[State <: Mutable, Gradient](evaluator: Evaluator[State, Gradient], maxEpochs: Int, valuePrecision: Double, stepPrecision: Double) {

  val DEBUG = false
  def debug(m: =>String): Unit = if (DEBUG) println(m)

  // TODO: move it into a config class or something
  // the rate at which step decreases on success
  val fastStepDecreaseQ = 1.0 / math.E
  val stepDecreaseQ = math.sqrt(0.5)
  private val maxProgress = 1.0 / (1.0 + stepDecreaseQ)
  val maxJumpsAlongGradient = 2

  private[ml] case class Cursor(functionValue: Double, step: Double, progress: Option[Double] = None, counter: Int = 0) {

    def enough: Boolean = {
      functionValue < valuePrecision || step < stepPrecision
    }

    def enoughProgress: Boolean = {
      counter >= maxJumpsAlongGradient || (progress exists (p => math.abs(p) < 1 + valuePrecision))
    }
  }
  
  def findAlongGradient(position: State, gradient: Gradient, initState: Cursor): Cursor = {
    debug(s"===fAG at $position, is=$initState")
    val result = Stream.iterate(initState) {
      s =>
        debug(s"@$s:\n  $position / $gradient ->")
        evaluator.nudge(position, gradient, -s.step)
        debug(s" $position")
        val newValue = evaluator.targetFunction(position, s.functionValue)
        val progress = newValue / s.functionValue
        val next = if (progress > 1 + valuePrecision) {
          evaluator.nudge(position, gradient, s.step)
          Cursor(s.functionValue, s.step * fastStepDecreaseQ, None, s.counter + 1)
        } else {
          Cursor(newValue, s.step, Some(progress), s.counter + 1)
        }
        next
    } find (_.enoughProgress) head

    debug(s"1. $result:\n $position")

    val finalResult = result.progress map {
      p =>
        val maxP = math.min(p, maxProgress)
        val lastStep = result.step * maxP / (1.0 - maxP)
        evaluator.nudge(position, gradient, -lastStep)
        val newValue = evaluator.targetFunction(position, result.functionValue)
        debug(s"nudged with $lastStep, got $position with value $newValue")
        if (newValue > result.functionValue) {
          evaluator.nudge(position, gradient, lastStep)
          result
        } else {
          Cursor(newValue, lastStep, Some(p), result.counter + 1)
        }
    } getOrElse result

    debug(s"2. $finalResult:\n $position")
    finalResult
  }

  def find(point: State, initStep: Double): Option[(Double, Int)] = {

    Stream.iterate((Cursor(evaluator.targetFunction(point), initStep), None: Option[Gradient], initStep, 0)) {
      case (s, previousGradient, step, epoch) =>
        val gradient = evaluator.gradientAt(point)
        debug(s"---at $point (${evaluator.targetFunction(point)}), epoch $epoch, step $step, status $s")
        val r0 = findAlongGradient(point, gradient, s).copy(step = s.step * stepDecreaseQ)
        val newEpoch = epoch + r0.counter
        val tentativeStep = previousGradient map ((g: Gradient) => {
          val cos = evaluator.cos(g, gradient)
          (1 + 0.3 * cos) * step
        }) getOrElse step
        val newStep = s.progress map (_ => tentativeStep) getOrElse r0.step

        val r = r0.copy(progress = None, counter = 0, step = newStep)
        debug(s"===new state=$r")

        (r, Some(gradient), newStep, newEpoch)
    } find { case (s, gradient, newStep, epoch) => epoch >= maxEpochs || s.enough } map { case (s, gradient, newStep, epoch) => (s.functionValue, epoch) }
  }
}

object GradientDescentEngine {

  /**
    * Evaluator for gradient descent, must implement all the abstractions
    *
    * @tparam Gradient  the type of space elements
    * @tparam State the type of mutable space elements
    */
  trait Evaluator[State <: Mutable, Gradient] {
    def cos(previousGradient: Gradient, gradient: Gradient): Double

    /**
      * Evaluates target function at given position
      *
      * @param position at which we evaluate the target function
      * @param maxValue maximum target function value, after which calculation can stop
      * @return target function value
      */
    def targetFunction(position: State, maxValue: Double): Double

    def targetFunction(position: State): Double = targetFunction(position, Double.MaxValue)

    /**
      * Calculates normalized gradient at a given position.
      * We assume we are in a linear space, so the gradient
      * is the same type Gradient
      *
      * @param position at which we calculate the gradient
      * @return gradient value
      */
    def gradientAt(position: State): Gradient

    /**
      * Nudges the current position, in place (so it's mutable somewhere inside)
      * <code>position += direction * step</code>
      *
      * @param position  current position; will change after nudging
      * @param direction aka gradient, the direction in which the position should be nudged
      * @param step      how far we should go.
      */
    def nudge(position: State, direction: Gradient, step: Double): Unit
  }

  implicit class DoubleVal(x0: Double) {
    def apply() = x0

    def copy = new DoubleVal(x0)

    override def toString = "" + x0
  }

  implicit class DoubleVar(var x: Double) extends DoubleVal(Double.NaN) with Mutable {
    override def apply() = x

    override def copy = new DoubleVal(x)

    override def toString = "" + x
  }

  def numericEvaluator(f: Double => Double, `f'`: Double => Double) = new Evaluator[DoubleVar, DoubleVal] {

    override def targetFunction(position: DoubleVar, maxValue: Double) = f(position())

    override def gradientAt(position: DoubleVar) = new DoubleVal(`f'`(position()))

    def targetFunction(position: Double): Double = targetFunction(new DoubleVar(position))

    def gradientAt(position: Double): DoubleVal = gradientAt(new DoubleVar(position))

    override def nudge(position: DoubleVar, direction: DoubleVal, step: Double) = {
      position.x += direction() * step
    }

    override def cos(previousGradient: DoubleVal, gradient: DoubleVal) = math.signum(previousGradient() * gradient())
  }

  def vectorEvaluator[Space <: VectorSpace](s: Space)(f: s.MutableVector => Double, `f'`: s.MutableVector => s.Vector): Evaluator[s.MutableVector, s.Vector] = new Evaluator[s.MutableVector, s.Vector] {

    override def targetFunction(position: s.MutableVector, maxValue: Double) = {
      val e = f(position)
      e
    }

    override def gradientAt(position: s.MutableVector) = `f'`(position)

    override def nudge(position: s.MutableVector, direction: s.Vector, step: Double) = {
      position.nudge(direction, step)
    }

    override def cos(previousGradient: s.Vector, gradient: s.Vector) = s.cos(previousGradient, gradient)
  }
}