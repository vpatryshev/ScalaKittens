package org.scalakittens

import org.scalakittens.Ops.tryOr
import org.scalakittens.Strings.{dropLeading, isEmpty}
import org.scalakittens.types.Person
import org.scalakittens.types.Money._

trait Validating[T] { self: T =>

  trait Condition {
    def check: Result[T]
  }

  def condition(predicate: => Boolean, message: => String): Condition = new Condition {
    def check: Result[Validating[T] with T] = tryOr(Good(self).filter((x:T) => predicate, message),
    (e: Exception) => Result.exception(e))
  }

  def condition[U](r: => Result[U]): Condition = new Condition {
    def check: Result[Validating[T] with T] = r map (_ => self)
  }

  def shouldBeEqual[X](actual: => X, expected: => X, name: String, additionalComments: String = ""): Condition = condition(
    actual == expected, desc(name, actual) + " should be <<" + expected + ">> " + additionalComments
  )

  def shouldBePresent(actual: => String, name: String): Condition = condition(
    !isEmpty(actual), desc(name, actual) + " should not be empty"
  )

  def trimName(s: String): String = s.replaceAll(" ", "").toUpperCase

  def namesShouldBeEqual(suggestedName: => String, person: => Person, varName: String): Condition = condition(person.sameNameAs(suggestedName), s"$varName <<$suggestedName>> should match the name of ${person.name}")

  def allValid[U](coll: => Traversable[Validating[U]]): Condition = new Condition {
    def check: Result[Validating[T] with T] = Result.traverse(coll.map(_.validate)).map(x => self) // ignoring the collection
  }

  trait MassChecker[U] {
    def checkThat(cs:U => Iterable[Condition]): Result[U]
  }

  implicit def applicable[U](res: Result[U]): MassChecker[U] = new MassChecker[U] {
    def checkAllValues(cs: Traversable[Condition]): Traversable[Result[T]] = cs.map(_.check)
    def checkThat(cs:U => Iterable[Condition]): Result[U] = {
      res.flatMap(value => Result.traverse(checkAllValues(cs(value))).flatMap(_ => res))
    }
  }

  def allValid[U](res: Result[Traversable[Validating[U]]]): Condition = {
    val validation: Result[T] = res.flatMap(coll => allValid(coll).check)
    condition(validation)
  }

  private def desc[X](name: String, x: X) = "the value of " + name + " <<" + x + ">>"

  private val trimZeroes = dropLeading(" 0")

  def shouldBeEqualUpToLeadingZeroes(actual: => String, expected: => String, name: String): Condition = condition(
    trimZeroes(actual) == trimZeroes(expected), desc(name, actual) + " should be <<" + expected + ">> (up to leading zeroes)"
  )

  def shouldBeTheTailOf(actual: => String, expected: => String, name: String): Condition = condition(
    expected endsWith actual, desc(name, expected) + " should end with <<" + actual + ">>"
  )

  def ocrSimplified(s: String): String = s.replaceAll("S", "5").replaceAll("l", "1").replaceAll("I", "1").replaceAll("O","0").replaceAll("o","0")

  def shouldNotExceed(actual: => BigDecimal, nameOfActual: String, limit: => BigDecimal, nameOfLimit: String): Condition = condition(
    actual <= limit, desc(nameOfActual, actual) + " should not exceed " + desc(nameOfLimit, limit)
  )

  def sanityCheckCanBeNegative(dollarAmount: => BigDecimal, name: String): Condition = condition(
   !dollarAmount.tooBig,
  if (dollarAmount.tooBig)    desc(name, dollarAmount) + " cannot be bigger than " + MaxValue
    else                      desc(name, dollarAmount) + " is plain insane"
  )

  def sanityCheck(dollarAmount: => BigDecimal, name: String): Condition = condition(
    !dollarAmount.isNegative && !dollarAmount.tooBig,
    if      (dollarAmount.isNegative) desc(name, dollarAmount) + " cannot be negative"
    else if (dollarAmount.tooBig)    desc(name, dollarAmount) + " cannot be bigger than " + MaxValue
    else                                 desc(name, dollarAmount) + " is plain insane"
  )

  def conditionsToCheck: Seq[Condition]

  def check(conditions: Seq[Condition]): Result[T] = {
    val results: Seq[Result[T]] = conditions map {
      c => {
        val r:Result[T] = c.check
        r
      }
    }

    val traversed: Result[Traversable[T]] = Result.traverse(results)
    traversed  map (_=>self)
  }

  def validate: Result[T] = check(conditionsToCheck)

  def reportErrors: Result[T] = validate

  def isValid: Boolean = validate.isGood
}
