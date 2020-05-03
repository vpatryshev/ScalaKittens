package org.scalakittens.types

import java.math.MathContext

import org.scalakittens.Logging._
import org.scalakittens.{Empty, Good, Result}

import scala.language.{implicitConversions, postfixOps}
import scala.util.matching.Regex

trait Money {

  sealed class SmartMoney(value:BigDecimal) {
    def isZero     :Boolean = value == ZeroDollars
    def notZero    :Boolean = value != ZeroDollars
    def isPositive :Boolean = value > ZeroDollars
    def isNegative :Boolean = value < ZeroDollars
    def tooBig     :Boolean = value > MaxValue
//    def cannotBeNegative
    def $: String = "$"+value
  }
  
  implicit def smartMoney(bd: BigDecimal): SmartMoney = new SmartMoney(bd)

  private lazy val MoneyRegex = "[^\\$\\d\\.]*(\\$?-?\\$?\\d*(\\.\\d{0,2})?).*".r
  val ZeroBucks = "0.00"
  lazy val ZeroDollars:BigDecimal = dollarAmountToBigDecimal("0.00")

  lazy val MaxValue: BigDecimal = dollarAmountToBigDecimal("9999999.99")
  // Consider it a mistake if a bill contains values of $10M or more
  lazy val SomeZero:Option[BigDecimal] = Some(ZeroDollars)

  def dollarsNonEmpty(digits: String): Result[BigDecimal] = if (digits.isEmpty) Empty else Result.attempt(Good(dollarAmountToBigDecimal(digits.trim.split(" ")(0).replace("$", ""))), s"failed to extract dollar amount from $digits")

  private[types] def plainDollars(s0: String) = {
    s0.replaceAll("[ ,]", "") match {
      case MoneyRegex(s, t) if s.replaceAll("[^\\d]", "").nonEmpty ⇒
        Result.forValue(dollarAmountToBigDecimal(s), s"failed to extract dollar amount from $s")
      case nothingLikeThat ⇒
        Result.error(s"Does not look like money: <<$s0>>")
    }
  }

  private lazy val NegativeFormat1 = "[^\\(]*\\(([^\\)]+)\\).*".r
  private lazy val NegativeFormat2 = "[^\\$]*\\$?-([\\d\\.]+)".r
  private lazy val NegativeFormat3 = "[^\\-]*-\\$([\\d\\.]+)".r

  def dollars(digits: String): Result[BigDecimal] =
    if (digits.isEmpty) Good(ZeroDollars) else {
      digits match {
        case NegativeFormat1(value) ⇒ plainDollars(value) map (-_)
        case NegativeFormat2(value) ⇒ plainDollars(value) map (-_)
        case NegativeFormat3(value) ⇒ plainDollars(value) map (-_)
        case value                  ⇒ plainDollars(value)
      }
    }

  def dollarsOrZero(digits: String): BigDecimal = dollars(digits) getOrElse ZeroDollars

  // TODO(vlad): ADD A TEST FOR THIS!!! THERE WAS A BUG!
  def positiveDollars(digits: String): Result[BigDecimal] = {
    val digitsOnly = digits.replaceAll("[+-]", "")
    if (digitsOnly.isEmpty) Good(ZeroDollars) else {
      plainDollars(digitsOnly)
    }
  }

  private def extractDollarsFromString(s: String) = {
    val trimmed = s.replaceAll("\\(", "").replaceAll("\\)","").replaceAll("\\$","").trim
    if (trimmed.isEmpty || trimmed == "-") ZeroDollars else dollarAmountToBigDecimal(trimmed)
  }

  // TODO(vlad): cf MoneyRegex
  lazy val MoneyPattern: Regex = "([-]?[\\d,]+(\\.\\d\\d?)?|\\.\\d+).*"r

  private def dollarAmountToBigDecimal(digits: String): BigDecimal = {
    val trimmed = digits.replaceAll("[\\(\\)\\/$\\s+, +]", "").trim
    trimmed match {
      case "-" ⇒ ZeroDollars
      case ""  ⇒ throw new IllegalArgumentException("Cannot convert an empty string to dollar amount")
      case MoneyPattern(sValue, _) ⇒
        try {
          val v1 = new java.math.BigDecimal(sValue, MathContext.DECIMAL64).setScale(2)
          val v2 = new scala.math.BigDecimal(v1)
          v2
        } catch {
          case e: Exception ⇒   // see "Scala in Depth", page 184: "Carefully catching exceptions"
            error(s"dollarAmountToBigDecimal can't convert <<$digits>>")
            throw e
           // TODO(vlad): get rid of exceptions
        }

      case basura ⇒
        error(s"dollarAmountToBigDecimal can't convert <<$digits>>"); throw new IllegalArgumentException(s"dollarAmountToBigDecimal can't convert <<$digits ($basura)>>")

    }
  }

  def Max(x: BigDecimal, y: BigDecimal): BigDecimal = if (x < y) y else x
  def Min(x: BigDecimal, y: BigDecimal): BigDecimal = if (x < y) x else y

}

object Money extends Money
