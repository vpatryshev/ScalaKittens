package scalakittens.ml

import scala.language.{postfixOps,reflectiveCalls}
import scalakittens.la.VectorSpace
import scalakittens.ml.GradientDescentEngine.Evaluator

/**
  * Pretty generic tool for gradient descent.
  * It knows nothing about how to calculate error, or how to calculate gradient.
  * It just does the search.
  * The fact that we are in a linear space is also irrelevant.
  * Created by vpatryshev on 7/13/17.
  */
case class GradientDescentEngine[T <: {def copy : T}, MT <: T with Mutable](evaluator: Evaluator[T, MT], maxEpochs: Int, errorPrecision: Double, stepPrecision: Double) {
  val stepIncreaseQ = math.sqrt(2.0)
  private val maxProgress = stepIncreaseQ / (1 + stepIncreaseQ)

  def goodProgress(progress: Option[Double]): Boolean = progress exists (1 - errorPrecision >)

  val maxEpochAlongGradient = 3

  case class State(error: Double, step: Double, progress: Option[Double] = None, epoch: Int = 0, lastAction: String = "forward") {
    if (step > 100) {
      println(s"what? step=$step")
    }

    def enough: Boolean = {
      val yes = error < errorPrecision || step < stepPrecision
      yes
    }

    def enoughProgress: Boolean = {
      val yes = epoch >= maxEpochAlongGradient || (progress exists (p => math.abs(p) < 1 + errorPrecision))
      yes
    }

    override def toString = s"State(error=$error, step=$step, progress=$progress, $lastAction)"
  }
  
  val DEBUG = true

  def debug(m: =>String): Unit = if (DEBUG) println(m)
  
  def findAlongGradient(position: MT, gradient: T, initState: State): State = {
    debug(s"===fAG at $position, is=$initState")
    val result = Stream.iterate(initState) {
      s =>
        debug(s"@$s:\n  $position / $gradient ->")
        evaluator.nudge(position, gradient, -s.step)
        debug(s" $position")
        val newError = evaluator.error(position, s.error)
        val progress = newError / s.error
        val next = if (progress > 1 + errorPrecision) {
          evaluator.nudge(position, gradient, s.step)
          State(s.error, s.step / math.E, None, s.epoch + 1)
        } else {
          State(newError, s.step, Some(progress), s.epoch + 1)
        }
        next
    } find (_.enoughProgress) head

    debug(s"1. $result:\n $position")

    val finalResult = result.progress map {
      p =>
        val maxP = math.min(p, maxProgress)
        val lastStep = result.step * maxP / (1.0 - maxP)
        evaluator.nudge(position, gradient, -lastStep)
        val newError = evaluator.error(position, result.error)
        debug(s"nudged with $lastStep, got $position with error $newError")
        if (newError > result.error) {
          evaluator.nudge(position, gradient, lastStep)
          result.copy(lastAction = "back")
        } else {
          State(newError, lastStep, Some(p), result.epoch + 1)
        }
    } getOrElse result

    debug(s"2. $finalResult:\n $position")
    finalResult
  }

  def find(point: MT, initStep: Double): Option[(Double, Int)] = {

    Stream.iterate((State(evaluator.error(point), initStep), None: Option[T], initStep, 0)) {
      case (s, previousGradient, step, epoch) =>
        val gradient = evaluator.gradientAt(point)
        debug(s"---at $point (${evaluator.error(point)}), epoch $epoch, step $step, status $s")
        val r0 = findAlongGradient(point, gradient, s).copy(step = s.step / stepIncreaseQ)
        val newEpoch = epoch + r0.epoch
        val tentativeStep = previousGradient map ((g: T) => {
          val cos = evaluator.cos(g, gradient)
          (1 + 0.3 * cos) * step
        }) getOrElse step
        val newStep = s.progress map (_ => tentativeStep) getOrElse r0.step

        val r = r0.copy(progress = None, epoch = 0, step = newStep)
        debug(s"===new state=$r")
        val newEntry: (State, T, T, Double, Int) = (r, point.copy, gradient, newStep, epoch)

        (r, Some(gradient), newStep, newEpoch)
    } find { case (s, gradient, newStep, epoch) => epoch >= maxEpochs || s.enough } map { case (s, gradient, newStep, epoch) => (s.error, epoch) }
  }
}

object GradientDescentEngine {

  /**
    * Evaluator for gradient descent, must implement all the abstractions
    *
    * @tparam T  the type of space elements
    * @tparam MT the type of mutable space elements
    */
  trait Evaluator[T, MT <: T with Mutable] {
    def cos(previousGradient: T, gradient: T): Double

    /**
      * Evaluates error at given position
      *
      * @param position at which we evaluate the error
      * @param maxValue maximum error value, after which calculation can stop
      * @return error value
      */
    def error(position: T, maxValue: Double): Double

    def error(position: T): Double = error(position, Double.MaxValue)

    /**
      * Calculates normalized gradient at a given position.
      * We assume we are in a linear space, so the gradient
      * is the same type T
      *
      * @param position at which we calculate the gradient
      * @return gradient value
      */
    def gradientAt(position: T): T

    /**
      * Nudges the current position, in place (so it's mutable somewhere inside)
      * <code>position += direction * step</code>
      *
      * @param position  current position; will change after nudging
      * @param direction aka gradient, the direction in which the position should be nudged
      * @param step      how far we should go.
      */
    def nudge(position: MT, direction: T, step: Double): Unit
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

  def numericEvaluator(f: Double => Double, `f'`: Double => Double) = new Evaluator[DoubleVal, DoubleVar] {

    override def error(position: DoubleVal, maxValue: Double) = f(position())

    override def gradientAt(position: DoubleVal) = new DoubleVal(`f'`(position()))

    def error(position: Double): Double = error(new DoubleVal(position))

    def gradientAt(position: Double): DoubleVal = gradientAt(new DoubleVal(position))

    override def nudge(position: DoubleVar, direction: DoubleVal, step: Double) = {
      position.x += direction() * step
    }

    override def cos(previousGradient: DoubleVal, gradient: DoubleVal) = math.signum(previousGradient() * gradient())
  }

  def vectorEvaluator[Space <: VectorSpace](s: Space)(f: s.Vector => Double, `f'`: s.Vector => s.Vector): Evaluator[s.Vector, s.MutableVector] = new Evaluator[s.Vector, s.MutableVector] {

    override def error(position: s.Vector, maxValue: Double) = {
      val e = f(position)
      e
    }

    override def gradientAt(position: s.Vector) = `f'`(position)

    override def nudge(position: s.MutableVector, direction: s.Vector, step: Double) = {
      position.nudge(direction, step)
    }

    override def cos(previousGradient: s.Vector, gradient: s.Vector) = s.cos(previousGradient, gradient)
  }
}