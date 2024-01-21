package org.scalakittens.types

import org.scalakittens.Result

/**
  * A Trait for handling values that are percentages
  */
trait Percentage {
  /**
    * Normalize a string of digits representing a percentage
    * to value between 0.0 through 1.0
    *
    * @param text A [[String]] of numeric digits followed by a %
    * @return A [[Result]] object with a  [[BigDecimal()]] object.
    */
  def parseAsNormalizedBigDecimal(text: String): Result[BigDecimal] = {
    try {
      val trimmedText = text.trim
      val TrailingPercentageRegExp = "(.+)%$".r
      val candidateDigits = trimmedText match {
        case TrailingPercentageRegExp(digits) => digits
        case _ => trimmedText
      }

      val number = BigDecimal(candidateDigits)
      // TODO(vlad, haroon): use the code I wrote in Frontend, it's more sophisitcated
      Result.forValue(number / 100.0)
    } catch {
      case e: Throwable =>
        val msg = s"""Exception while parsing "$text" : ${e.getMessage}"""
        Result.error[BigDecimal](msg)
    }
  }
}

object Percentage extends Percentage
