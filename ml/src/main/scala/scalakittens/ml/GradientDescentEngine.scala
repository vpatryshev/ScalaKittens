package scalakittens.ml

import scala.language.postfixOps
import scalakittens.Tracker
import scalakittens.la.VectorSpace
import scalakittens.ml.GradientDescentEngine.Evaluator

/**
  * Pretty generic tool for gradient descent.
  * It knows nothing about how to calculate target function, or how to calculate gradient.
  * It just does the search.
  * The fact that we are in a linear space is also irrelevant.
  * Created by vpatryshev on 7/13/17.
  * 
  * Type parameters: 
  *   State, which represents current search positions
  *   Tangent, which represent search directions @see https://en.wikipedia.org/wiki/Tangent_space
  */
sealed case class GradientDescentEngine[State <: Mutable, Tangent](evaluator: Evaluator[State, Tangent], maxEpochs: Int, valuePrecision: Double, stepPrecision: Double) {

  val DEBUG = false
  def debug(m: ⇒String): Unit = if (DEBUG) println(m)

  // TODO: move it into a config class or something
  // the rate at which step decreases on success
  private val fastStepDecreaseQ = 1.0 / math.E
  private val stepDecreaseQ = math.sqrt(0.5)
  private val maxProgress = 1.0 / (1.0 + stepDecreaseQ)
  val maxJumpsAlongGradient = 2

  private[ml] case class Cursor(functionValue: Double, step: Double, progress: Option[Double] = None, counter: Int = 0) {

    def enough: Boolean = {
      functionValue < valuePrecision || step < stepPrecision
    }

    def enoughProgress: Boolean = {
      counter >= maxJumpsAlongGradient || (progress exists (p ⇒ math.abs(p) < 1 + valuePrecision))
    }
  }
  
  def findAlongGradient(position: State, gradient: Tangent, initState: Cursor): Cursor = {
    val tracker = new Tracker
    debug(s"===fAG at $position, is=$initState")
    val result = Stream.iterate(initState) {
      s ⇒
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

    debug(s"1. $result")

    val finalResult = result.progress map {
      p ⇒
        val maxP = math.min(p, maxProgress)
        val lastStep = result.step * maxP / (1.0 - maxP)
        evaluator.nudge(position, gradient, -lastStep)
        val newValue = evaluator.targetFunction(position, result.functionValue)
        debug(s"nudged with $lastStep, got $position with value $newValue")
        if (newValue > result.functionValue) {
          evaluator.nudge(position, gradient, lastStep)
          debug("nudged back")
          result
        } else {
          debug("some progress")
          Cursor(newValue, lastStep, Some(p), result.counter + 1)
        }
    } getOrElse {
      debug("no progress")
      result
    }

    debug(s"2. $finalResult")
    if (DEBUG) tracker << s"gde along gradient: ${finalResult.functionValue}, ${finalResult.step}"
    finalResult
  }

  def find(point: State, initStep: Double): Option[(Double, Int)] = {

    Stream.iterate((Cursor(evaluator.targetFunction(point), initStep), None: Option[Tangent], false, initStep, 0)) {
      case (s, previousGradient, sameGradient, step, epoch) ⇒
        val gradient = previousGradient filter (_ ⇒ sameGradient) getOrElse evaluator.gradientAt(point)
        debug(s"---at $point (${evaluator.targetFunction(point)}), grad=$gradient, epoch $epoch, step $step, status $s")
        val r0 = findAlongGradient(point, gradient, s).copy(step = s.step * stepDecreaseQ)
        val newEpoch = epoch + r0.counter
        def tentativeStep = previousGradient map ((g: Tangent) ⇒ {
          val cos = evaluator.cos(g, gradient)
          debug(s"cos = $cos, prog=${s.progress}")
          (1 + 0.3 * cos) * step
        }) getOrElse step
        val newStep = s.progress map (_ ⇒ tentativeStep) getOrElse r0.step

        val r = r0.copy(progress = None, counter = 0, step = newStep)
        debug(s"===new state=$r @$point")
        (r, Some(gradient), r0.progress.isEmpty, newStep, newEpoch)
    }. 
    find { case (s, gradient, sameGrad, newStep, epoch) ⇒ epoch >= maxEpochs || s.enough }. 
    map  { case (s, gradient, sameGrad, newStep, epoch) ⇒ (s.functionValue, epoch) }
  }
}

object GradientDescentEngine {

  /**
    * Evaluator for gradient descent, must implement all the abstractions
    *
    * @tparam Tangent  the type of space elements
    * @tparam State the type of mutable space elements
    */
  trait Evaluator[State <: Mutable, Tangent] {
    def cos(gradient1: Tangent, gradient2: Tangent): Double

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
      * is the same type Tangent
      *
      * @param position at which we calculate the gradient
      * @return gradient value
      */
    def gradientAt(position: State): Tangent

    /**
      * Nudges the current position
      * <code>position + direction * step</code>
      *
      * @param position  current position; will change after nudging
      * @param direction aka gradient, the direction in which the position should be nudged
      * @param step      how far we should go.
      */
    def nudge(position: State, direction: Tangent, step: Double): Unit
  }

  implicit class DoubleVal(x0: Double) {
    def apply(): Double = x0

    def copy = new DoubleVal(x0)

    override def toString: String = "" + x0
  }

  implicit class DoubleVar(var x: Double) extends DoubleVal(Double.NaN) with Mutable {
    override def apply(): Double = x

    override def copy = new DoubleVal(x)

    override def toString: String = "" + x
  }

 def numericEvaluator(f: Double ⇒ Double, `f'`: Double ⇒ Double): Evaluator[DoubleVar, DoubleVal] 
 = new Evaluator[DoubleVar, DoubleVal] {

    override def targetFunction(position: DoubleVar, maxValue: Double) = f(position())

    override def gradientAt(position: DoubleVar) = new DoubleVal(`f'`(position()))

//    def targetFunction(position: Double): Double = targetFunction(new DoubleVar(position))

//    def gradientAt(position: Double): DoubleVal = gradientAt(new DoubleVar(position))

    override def nudge(position: DoubleVar, direction: DoubleVal, step: Double): Unit
    = {
      position.x += direction() * step
    }

    override def cos(previousGradient: DoubleVal, gradient: DoubleVal): Double
    = math.signum(previousGradient() * gradient())
  }

  def vectorEvaluator[Space <: VectorSpace](s: Space)(f: s.MutableVector ⇒ Double, `f'`: s.MutableVector ⇒ s.Vector): Evaluator[s.MutableVector, s.Vector] = new Evaluator[s.MutableVector, s.Vector] {

    override def targetFunction(position: s.MutableVector, maxValue: Double): Double
    = f(position)

    override def gradientAt(position: s.MutableVector): s.Vector = `f'`(position)

    override def nudge(position: s.MutableVector, direction: s.Vector, step: Double): Unit 
    = position.nudge(direction, step)

    override def cos(previousGradient: s.Vector, gradient: s.Vector): Double = s.cos(previousGradient, gradient)
  }
}