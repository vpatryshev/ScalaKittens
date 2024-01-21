

package scalakittens.types

import java.math.MathContext
import scalakittens.Functions._
import scalakittens.types.Money._
import scalakittens.testing.TestBase
import org.specs2.matcher.MatchResult
import scalakittens.{Good, Result}

// TODO(vlad): fill it in, add as many tests as there are functions
class MoneyTest extends TestBase {
  def bd(s: String): Result[BigDecimal] = {
    val v1 = new java.math.BigDecimal(s, MathContext.DECIMAL64).setScale(2)
    val v2 = new scala.math.BigDecimal(v1)
    Good(v2)
  }

  def d(s: String): BigDecimal = bd(s) getOrElse ZeroDollars

  "plainDollars" should {
    "not give 0 on 'Total'" in {
      plainDollars("Total").isBad must beTrue
    }
  }

  "dollars()" should {
    "work" in {

      val x = dollars("$-734.00")
      x must_== bd("-734.00")
      val function: scalakittens.Function[String, Result[BigDecimal]] = dollars _ named "dollars"
      val result: MatchResult[Any] = function maps(
        "0" -> bd("0.00"),
        ".00" -> bd("0.00"),
        "$42.00" -> bd("42.00"),
        "  68. " -> bd("68.00"),
        " 42.0" -> bd("42.00"),
        "33.1" -> bd("33.1"),
        "1,230.5" -> bd("1230.50"),
        " just 0 " -> bd("0.00"),
        "Vineet pays me $10000.00 an hour" -> bd("10000.00"),
        "got my salary, it is $ 2 0 0 bucks" -> bd("200.00"),
        "1,000,000.00" -> bd("1000000.00"),
        "$-734.00" -> bd("-734.00"),
        "-$734.00" -> bd("-734.00"),
        "-0.01" -> bd("-.01"))
      result
    }
    "fail" in {
      dollars("Totals").isBad must beTrue
// this is questionable      dollars("").isBad must beTrue
    }

    "dollarsOrZero()" should {
      "work" in {
        val function: scalakittens.Function[String, BigDecimal] = dollarsOrZero _ named "dollarsOrZero"
        val result: MatchResult[Any] = function maps(
          "0" -> d("0.00"),
          "$42.00" -> d("42.00"),
          "  68. " -> d("68.00"),
          " 42.0" -> d("42.00"),
          "33.1" -> d("33.1"),
          "1,230.5" -> d("1230.50"),
          " just 0 " -> d("0.00"),
          "Vineet pays me $10000.00 an hour" -> d("10000.00"),
          "got my salary, it is $ 2 0 0 bucks" -> d("200.00"),
          "1,000,000.00" -> d("1000000.00"),
          "$-734.00" -> d("-734.00"),
          "-$734.00" -> d("-734.00"),
          "-0.01" -> d("-.01"),
          "Total|Deductions|-$583.50" -> d("-583.50"),
          "$$" -> ZeroDollars,
          "how much?" -> ZeroDollars,
          "" -> ZeroDollars)
        result
      }
    }
  }

}
