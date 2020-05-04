package org.scalakittens.types

import java.util.Date
import java.util.concurrent.TimeUnit

import com.github.nscala_time.time.Imports._
import org.joda.time.DateTime
import org.scalakittens.{DateAndTime, Good, Result, TimeReader}
import org.scalakittens.Result.Outcome
import org.scalakittens.DateAndTime._
import org.joda.time.format.DateTimeFormatter

import scala.concurrent.duration.Duration

/**
 * Person's date of birth
 *
 * TODO(vlad): use DateTime from Joda @see https://github.com/nscala-time/nscala-time/ to compare dates etc
 */
abstract class DateOfBirth(date:DateTime) {
  def millis: Long = date.getMillis
  def currentTime: Long
  def age: Duration = Duration(currentTime - millis, TimeUnit.MILLISECONDS)
  def sameAs(other: DateOfBirth): Boolean = math.abs(millis - other.millis) < MILLIS_IN_A_DAY * 2
  def probablyAnError: Boolean = millis < tooEarlyDOB || millis > tooLateDOB
  private def tooEarlyDOB = currentTime - 120 * MILLIS_IN_A_YEAR
  private def tooLateDOB  = currentTime -   7 * MILLIS_IN_A_DAY

  override def toString = s"DOB=${DateOfBirth.asString(date)}"
  override def equals(x: Any): Boolean = x match {
    case other: DateOfBirth ⇒ sameAs(other)
    case bs ⇒ false
  }
  override def hashCode: Int = 13 * millis.hashCode()
}

private[types] trait DobBuilder extends TimeReader {
  private def earliestDOB = currentTime - 140 * MILLIS_IN_A_YEAR

  private def validateTob(tob: Long): Outcome = {
    val asString = DateOfBirth.asString(new DateTime(tob))
    Good(tob).
      filter(earliestDOB < (_:Long), s"Birth date too early: $asString").
      filter(currentTime > (_:Long), s"Birth date in the future: $asString")
  }

  def apply(time: Long): Result[DateOfBirth] = validateTob(time) returning DateOfBirth(new DateTime(time))

  def apply(date: Date): Result[DateOfBirth] = apply(date.getTime)

  def apply(dob: Option[Long]): Result[DateOfBirth] = Result(dob, "DOB missing") flatMap apply

  def apply(dob: String): Result[DateOfBirth] = extractDate(dob) flatMap apply

  def sameDob(dob1: Result[DateOfBirth])(dob2: Result[DateOfBirth]): Boolean = (dob1, dob2) match {
    case (Good(first), Good(second)) ⇒ first sameAs second
    case (x, y)               ⇒ x.isBad == y.isBad
  }
}

object DateOfBirth extends DobBuilder {
  def apply(dateTime: DateTime): DateOfBirth = new DateOfBirth(dateTime) {
    def currentTime: Long = DateAndTime.currentTime
  }

  val dateFormat: DateTimeFormatter = DateTimeFormat.forPattern("MM/dd/yyyy")
  def asString(date: DateTime): String = dateFormat print date
  def apply(date: java.util.Date, currentTime0: Long = DateAndTime.currentTime): DateOfBirth = {
    val millis = date.getTime
    new DateOfBirth(new DateTime(millis)) {
      def currentTime: Long = currentTime0
    }
  }
}
