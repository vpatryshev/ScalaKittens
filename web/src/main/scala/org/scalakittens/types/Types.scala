package org.scalakittens.types

import org.scalakittens.Result.{OK, Outcome}
import org.scalakittens.web.Url
import org.scalakittens.{Good, Result}

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}

// makes sense to declare common types somewhere... e.g. here
trait Types {
  type predicate[-P] = Function1[P, Boolean]
  type predicate2[-P1, -P2] = Function2[P1, P2, Boolean]
  type richPredicate[-P] = Function1[P, Outcome]
  type richPredicate2[-P1, -P2] = Function2[P1, P2, Outcome]

  type IDof[T] = Long
  def emptyID[T]: IDof[T] = 0L
  def someEmptyID[T]: Option[IDof[T]] = Some(emptyID[T])

  def idOf[T](s: String): Result[IDof[T]] =
    Result.forValue(s.split(" ").toList.filter(_.nonEmpty).head.toLong)
  
  def idOf[T](n: Long): IDof[T] = n:IDof[T]

  trait Identifiable[T] {
    def id: IDof[T]

    def hasID: Boolean = id != emptyID // think about having a separate type for the stuff that does not have an id... challenging
  }

  type LoginUrl = Url
  type PdfUrl = Url

  def ???(implicit message: String = "not implemented"): Nothing = sys.error(message)
  implicit def betterArray[T](array: Array[T]):BetterArray[T] = new BetterArray[T](array)

  class BetterArray[T](array: Array[T])/* extends Seq[T] */{
    def get(i: Int): Option[T] = Result.attempt(Good(array(i))).toOption
    def getOrElse[T1<:T](i: Int, defaultValue: =>T1): T = get(i) getOrElse defaultValue
    def apply(i: Int): T = get(i).get
  }

  trait ArrayOf[Element] {
    def contents: Seq[Element]
    def size: Int
    def check(i: Int): Unit = {
      if (i < 0) throw new IndexOutOfBoundsException("index must be >=0: " + i)
      if (i >= size) {
        throw new IndexOutOfBoundsException("index must be < " + size + ", got " + i + ": " + this)
      }
    }

    def apply(i: Int): Element = {check(i); contents(i)}
    def getOr(i: Int, alt: Element): Element = if (i < size) contents(i) else alt
    def where(predicate: Element => Boolean): Result[Int] = {
      Good(contents indexWhere predicate) filter ((_:Int) >= 0, "Nothing found")
    }

    def has(predicate: Element => Boolean): Boolean = this where predicate isGood
  }

  def extractListOfLists(list: List[_]): List[List[String]] = list .collect { case cols: List[_] => cols .map (_.toString) }

  // use it in lazy outcomes
  val DoNothing: () => Result.OK.type = () => OK
  val GoodAsIs = Good(DoNothing)

}

object Types extends Types with Metamorphoses
