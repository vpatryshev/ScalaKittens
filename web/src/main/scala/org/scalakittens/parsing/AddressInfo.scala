package org.scalakittens.parsing

import org.scalakittens.nlp.Linguist._
import org.scalakittens.{Good, Result}

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

case class AddressInfo(name: String, address: Option[String])

object AddressInfo {
  def apply(text: String): Result[AddressInfo] = parse(text)

  private val parse = new RegexParsers {
    //    override def skipWhitespace = false
    def number: Regex = "#?\\d+".r

    def text: Regex = "[^#\\d]+".r

    def restOfAddressExp: Regex = ".*".r

    def addressExp: Parser[AddressInfo] = text ~ rep(number) ~ restOfAddressExp ^^ {
      case name ~ numbers ~ restOfAddress ⇒
        if (!restOfAddress.isEmpty) {
          // move the last number to address line only if some continuation is following. Just to avoid an address consisting from the number only
          val n = name :: numbers.dropRight(1) mkString " "
          val a = numbers.lastOption.map(_ + " " + restOfAddress)
          AddressInfo(toTitleCase(n), a)
        } else {
          val n = name :: numbers mkString " "
          AddressInfo(toTitleCase(n), None)
        }
    }

    def apply(s0: String): Result[AddressInfo] = {
      val s = Option(s0).getOrElse("").trim.replaceAll(" ,", ",")
      val result =
        if (s.isEmpty) Result.error("Address Missing")
        //      else if (s.containsIgnoreCase("not available")) Result.error("not available")
        else parseAll(addressExp, s) match {
          case Success(x, _) ⇒ Good(x)
          case NoSuccess(x, y) ⇒ Good(AddressInfo(toTitleCase(s), None))
        }

      result
    }
  }

}
