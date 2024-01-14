package scalakittens.types

import scalakittens.{Good, Result}
import scalakittens.Result._

import scala.languageFeature.{implicitConversions, postfixOps}

// makes sense to declare common types somewhere... e.g. here
trait Types {
  type Predicate[-P] = Function1[P, Boolean]
  type Predicate2[-P1, -P2] = Function2[P1, P2, Boolean]
  type RichPredicate[-P] = Function1[P, Outcome]
  type RichPredicate2[-P1, -P2] = Function2[P1, P2, Outcome]


  def ???(implicit message: String = "not implemented"): Nothing = sys.error(message)
  implicit def betterArray[T](array: Array[T]):BetterArray[T] = new BetterArray[T](array)

  class BetterArray[T](array: Array[T])/* extends Seq[T] */{
    def get(i: Int): Option[T] = Result.attempt(Good(array(i))).asOption
    def getOrElse[T1<:T](i: Int, defaultValue: =>T1) = get(i) getOrElse defaultValue
    def apply(i: Int): T = get(i).get
  }

  trait ArrayOf[Element] {
    def contents: Seq[Element]
    def size: Int
    def check(i: Int) = {
      if (i < 0) throw new IndexOutOfBoundsException("index must be >=0: " + i)
      if (i >= size) {
        throw new IndexOutOfBoundsException("index must be < " + size + ", got " + i + ": " + this)
      }
    }

    def apply(i: Int) = {check(i); contents(i)}
    def getOr(i: Int, alt: Element) = if (i < size) contents(i) else alt
    def where(predicate: Element => Boolean): Result[Int] = {
      Good(contents indexWhere predicate) filter ((_:Int) >= 0, "Nothing found")
    }

    def has(predicate: Element => Boolean): Boolean = this where predicate isGood
  }

  def extractListOfLists(list: List[_]) = list .collect { case cols: List[_] => cols .map (_.toString) }

  // use it in lazy outcomes
  val DoNothing = () => OK
  val GoodAsIs = Good(DoNothing)

}

object Types extends Types with Metamorphoses
