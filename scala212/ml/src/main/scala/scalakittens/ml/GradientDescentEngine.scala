package scalakittens.ml

import language.postfixOps
import scalakittens.la.VectorSpace
import scalakittens.ml.GradientDescentEngine.Evaluator

/** 
  * Pretty generic tool for gradient descent.
  * It knows nothing about how to calculate error, or how to calculate gradient.
  * It just does the search.
  * The fact that we are in a linear space is also irrelevant.
  * Created by vpatryshev on 7/13/17.
  */
case class GradientDescentEngine[T, MT <: T with Mutable](evaluator: Evaluator[T, MT], maxEpochs: Int, errorPrecision: Double) {
  val stepDecreaseQ = math.sqrt(2.0)

  def goodProgress(progress: Option[Double]): Boolean = progress exists (1-errorPrecision >)
  
  case class State(error: Double, step: Double, progress: Option[Double] = None, epoch: Int = 0) {
    def enough: Boolean = {
      val enoughProgress = progress exists (p => math.abs(p - 1) < errorPrecision)
      error < errorPrecision || epoch >= maxEpochs || enoughProgress
    }
    
    def shouldGoBack: Boolean = {
      epoch < maxEpochs && (progress forall (p => p > 1 + errorPrecision))
    }

    override def toString = s"State(error=$error, step=$step, progress=$progress, epoch=$epoch)"
  }
  
   def findAlongGradient(position: MT, gradient: T, initState: State): State = {
     val result = Stream.iterate(initState) {
       s =>
         println(s"@$s:\n$position/$gradient/${s.step} ->")
         evaluator.nudge(position, gradient, -s.step)
         println(s"$position/$gradient/${s.step}")
         val newError = evaluator.error(position, s.error)
         val progress = newError / s.error
         if (progress > 1 + errorPrecision) {
           evaluator.nudge(position, gradient, s.step)
           State(s.error, s.step / 2, Some(progress), s.epoch+1)
         } else {
           State(newError, s.step, Some(progress), s.epoch+1)
         }
     } dropWhile (s => s.shouldGoBack) head

     println(s"@$result:\n$position/$gradient/${result.step} ->")
     println(s"$position/$gradient/${result.step}")
     
     result.progress map {
       p =>
         val lastStep = result.step * p / (1.0 - p)
         evaluator.nudge(position, gradient, -lastStep)
         val newError = evaluator.error(position, result.error)
         if (newError > result.error) {
           evaluator.nudge(position, gradient, lastStep)
           result
         } else {
           State(newError, lastStep, Some(newError / result.error), result.epoch+1)
         }
     } getOrElse result
   }
  
  def find(point: MT, initStep: Double): Option[(Double, Int)] = {
    Stream.iterate(State(evaluator.error(point), initStep)) {
      s =>
        val gradient = evaluator.gradientAt(point)
        findAlongGradient(point, gradient, s)
    } .dropWhile (!_.enough) .headOption .map (s => (s.error, s.epoch))
  }
}

object GradientDescentEngine {

  /**
    * Evaluator for gradient descent, must implement all the abstractions
    * @tparam T the type of space elements
    * @tparam MT the type of mutable space elements
    */
  trait Evaluator[T, MT <: T with Mutable] {
    /**
      * Evaluates error at given position
      * @param position at which we evaluate the error
      * @param maxValue maximum error value, after which calculation can stop
      * @return error value
      */
    def error(position: T, maxValue: Double): Double
    def error(position: T): Double = error(position, Double.MaxValue)

    /**
      * Calculates gradient at a given position.
      * We assume we are in a linear space, so the gradient
      * is the same type T
      * @param position at which we calculate the gradient
      * @return gradient value
      */
    def gradientAt(position: T): T

    /**
      * Nudges the current position, in place (so it's mutable somewhere inside)
      * <code>position += direction * step</code>
      * 
      * @param position current position; will change after nudging
      * @param direction aka gradient, the direction in which the position should be nudged
      * @param step how far we should go.
      */
    def nudge(position: MT, direction: T, step: Double): Unit
  }

  implicit class DoubleVal(x0: Double) {
    def apply() = x0
    override def toString = ""+x0
  }
  
  implicit class DoubleVar(var x: Double) extends DoubleVal(Double.NaN) with Mutable {
    override def apply() = x
    override def toString = ""+x
  }

  def numericEvaluator(f: Double => Double, `f'`: Double => Double) = new Evaluator[DoubleVal, DoubleVar] {
    
    override def error(position: DoubleVal, maxValue: Double) = f(position())

    override def gradientAt(position: DoubleVal) = new DoubleVal(`f'`(position()))

    def error(position: Double): Double = error(new DoubleVal(position))

    def gradientAt(position: Double): DoubleVal = gradientAt(new DoubleVal(position))

    override def nudge(position: DoubleVar, direction: DoubleVal, step: Double) = {
      position.x += direction() * step
    }
}
  
  def vectorEvaluator[Space <: VectorSpace](s: Space)(f: s.Vector => Double, `f'`: s.Vector => s.Vector): Evaluator[s.Vector, s.MutableVector] = new Evaluator[s.Vector, s.MutableVector] {

    override def error(position: s.Vector, maxValue: Double) = f(position)

    override def gradientAt(position: s.Vector) = `f'`(position)

    override def nudge(position: s.MutableVector, direction: s.Vector, step: Double) = {
      position.nudge(direction, step)
    }
  }
}