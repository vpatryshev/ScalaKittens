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
case class GradientDescentEngine[T, MT <: T](evaluator: Evaluator[T, MT], maxEpochs: Int, errorPrecision: Double) {
  val stepDecreaseQ = math.sqrt(2.0)

  def goodProgress(progress: Option[Double]): Boolean = progress exists (1-errorPrecision >)
  
  case class State(error: Double, step: Double, progress: Option[Double] = None, epoch: Int = 0) {
    def enough: Boolean = {
      val enoughProgress = progress exists (p => math.abs(p - 1) < errorPrecision)
      epoch >= maxEpochs || enoughProgress
    }
    
    override def toString = s"State(error=$error, step=$step, progress=$progress, epoch=$epoch)"
  }
  
   def findAlongGradient(position: MT, gradient: T, initState: State): State = {
     Stream.iterate(initState) {
       s => 
         println(s"@$s:\n$position/$gradient/${s.step} ->")
         evaluator.nudge(position, gradient, s.step)
         println(s"$position/$gradient/${s.step}")
//         evaluator.nudge(position, gradient, -s.step)
//         println(s" << $position/$gradient/${s.step}")
//         evaluator.nudge(position, gradient, s.step)
//         println(s" >> $position/$gradient/${s.step}")
         val newError = evaluator.error(position, s.error)
         val bigStep = s.step * stepDecreaseQ
         val smallStep = s.step / stepDecreaseQ
         val progress = Some(newError / s.error)
         println(s"  new progress=$progress, new error = $newError, new step = $bigStep, we are at $position")
         
         if (progress.isEmpty || goodProgress(progress)) {
           println(s"    applying $bigStep")
           State(newError, -bigStep, progress, s.epoch+1)
         } else {
           print(s"    moving back to... ")
           evaluator.nudge(position, gradient, -s.step)
           println(s"$position/$gradient/${s.step}")
           State(s.error, smallStep, progress, s.epoch+1)             
         }
     } dropWhile (!_.enough) head
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
  trait Evaluator[T, MT <: T] {
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
  
  def vectorEvaluator[Space <: VectorSpace](s: Space)(f: s.Vector => Double, `f'`: s.Vector => s.Vector): Evaluator[s.Vector, s.MutableVector] = new Evaluator[s.Vector, s.MutableVector] {
    /**
      * Evaluates error at given position
      *
      * @param position at which we evaluate the error
      * @param maxValue maximum error value, after which calculation can stop
      * @return error value
      */
    override def error(position: s.Vector, maxValue: Double) = f(position)

    /**
      * Calculates gradient at a given position.
      * We assume we are in a linear space, so the gradient
      * is the same type T
      *
      * @param position at which we calculate the gradient
      * @return gradient value
      */
    override def gradientAt(position: s.Vector) = `f'`(position)

    /**
      * Nudges the current position, in place (so it's mutable somewhere inside)
      * <code>position += direction * step</code>
      *
      * @param position  current position; will change after nudging
      * @param direction aka gradient, the direction in which the position should be nudged
      * @param step      how far we should go.
      */
    override def nudge(position: s.MutableVector, direction: s.Vector, step: Double) = {
      position.nudge(direction, step)
    }
}
}