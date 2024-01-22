package scalakittens

/**
  * A trait that describe a goodness of data or outcome or something
  *
  * Created by vpatryshev on 10/18/15.
  */
trait Goodness {
  def isGood: Boolean
  def isBad: Boolean = !isGood
}

trait PositiveAttitude extends Goodness { def isGood = true }
trait NegativeAttitude extends Goodness { def isGood = false}

