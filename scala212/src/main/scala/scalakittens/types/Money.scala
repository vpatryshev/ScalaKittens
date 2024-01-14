package scalakittens.types

import scalakittens.{Empty, Good, Result}

import java.math.MathContext
import scala.languageFeature.{implicitConversions, postfixOps}

trait Money {
  sealed class SmartMoney(value:BigDecimal) {
    def isZero     :Boolean = value == ZeroDollars
    def notZero    :Boolean = value != ZeroDollars
    def isPositive :Boolean = value > ZeroDollars
    def isNegative :Boolean = value < ZeroDollars
    def tooBig     :Boolean = value > MaxMoneyValue
//    def cannotBeNegative
    def $: String = "$"+value
  }
  
  implicit def smartMoney(bd: BigDecimal): SmartMoney = new SmartMoney(bd)

  private lazy val MoneyRegex = "[^\\$\\d\\.]*(\\$?-?\\$?\\d*(\\.\\d{0,2})?).*".r
  val ZeroBucks = "0.00"
  private lazy val ZeroDollars:BigDecimal = dollarAmountToBigDecimal("0.00").iHope

  def MaxMoneyValue: BigDecimal = BigDecimal("99999999999999.99")

  def dollarsNonEmpty(digits: String): Result[BigDecimal] =
    if (digits.isEmpty) Empty else
      dollarAmountToBigDecimal(digits.trim.split(" ")(0).replace("$", ""))

  private[types] def plainDollars(s0: String) = {
    s0.replaceAll("[ ,]", "") match {
      case MoneyRegex(s, t) if s.replaceAll("[^\\d]", "").nonEmpty =>
        dollarAmountToBigDecimal(s)
      case nothingLikeThat =>
        Result.error(s"Does not look like money: <<$s0>>")
    }
  }

  private lazy val NegativeFormat1 = "[^\\(]*\\(([^\\)]+)\\).*".r
  private lazy val NegativeFormat2 = "[^\\$]*\\$?-([\\d\\.]+)".r
  private lazy val NegativeFormat3 = "[^\\-]*-\\$([\\d\\.]+)".r

  def dollars(digits: String): Result[BigDecimal] =
    if (digits.isEmpty) Good(ZeroDollars) else {
      digits match {
        case NegativeFormat1(value) => plainDollars(value) map (-_)
        case NegativeFormat2(value) => plainDollars(value) map (-_)
        case NegativeFormat3(value) => plainDollars(value) map (-_)
        case value                  => plainDollars(value)
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
  lazy val MoneyPattern = "([-]?[\\d,]+(\\.\\d\\d?)?|\\.\\d+).*"r

  private def dollarAmountToBigDecimal(digits: String): Result[BigDecimal] = {
    val trimmed = digits.replaceAll("[\\(\\)\\/$\\s+, +]", "").trim
    trimmed match {
      case "-" => Some(ZeroDollars)
      case ""  => throw new IllegalArgumentException("Cannot convert an empty string to dollar amount")
      case MoneyPattern(sValue, _) =>
        Result.forValue(new java.math.BigDecimal(sValue, MathContext.DECIMAL64).setScale(2),
            s"dollarAmountToBigDecimal can't convert <<$digits>>")

      case basura =>
        Result.error(s"dollarAmountToBigDecimal can't convert <<$digits>>"); throw new IllegalArgumentException(s"dollarAmountToBigDecimal can't convert <<$digits ($basura)>>")

    }
  }

  def Max(x: BigDecimal, y: BigDecimal): BigDecimal = if (x < y) y else x
  def Min(x: BigDecimal, y: BigDecimal): BigDecimal = if (x < y) x else y

}

object Money extends Money
