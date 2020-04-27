package org.scalakittens.web

import java.util.Date

import org.openqa.selenium.Cookie
import org.scalakittens.{DateAndTime, Good, Logging, Result}

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * Encapsulates cookies
  */
class CookieSet(val cookies: Set[Cookie]) {
  override def toString: String = cookies.toList.mkString("[", ", ", "]")
}

object CookieSet extends DateAndTime {
  val parse: RegexParsers = new RegexParsers {
    override def skipWhitespace = false

    def name: Regex = "\"?([^\",;=]+)\"?".r

    def text: Regex = "[^;]*".r

    def nonCurlie: Regex = "[^},]*".r

    def date: Regex = "\\w{3}, \\d{2} \\w{3} \\d{4} \\d{2}:\\d{2}:\\d{2} \\w{3}".r

    /*    def json = "{" ~> repsep(value, ",") <~ "}" ^^ {
          case inside:Seq[String] =>
            "{" + inside.map(_.toString).mkString(",") + "}"
        }*/
    def value: Regex = /*json |*/ text

    def kvPair: Parser[(String, String)] = name ~ "=" ~ value ^^ {
      case k ~ _ ~ v => k -> v.toString
    }

    def option(name: String, fmt: Regex = text): Parser[String] = (s"; ?$name=".r ~> fmt ?) ^^ { opt => opt orNull }

    def cookieExp: Parser[Cookie] = kvPair ~ option("expires", date) ~ option("path") ~ option("domain") ~ (("; " +
      "?secure;").r ?) ^^ {
      case nvPair ~ expires ~ path ~ domain ~ secureOpt => // ~ secureOpt */ =>
        val date = Result.forValue(expires) flatMap dateFormat getOrElse null
        val cookie = new Cookie(nvPair._1, nvPair._2, domain, Option(path) getOrElse "/", date, secureOpt.isDefined)
        cookie
    }

    def apply(s0: String): Result[CookieSet] = {
      val s1 = s0.replaceAll("\\n", "")
      if (s1.length > 1 && s1.head == '[' && s1.last == ']') {
        val s = s1.substring(1, s1.length - 1)
        extractFrom(s)
      } else Result.error(s"cookie string should be wrapped in square brackets: $s0")
    }

    private def extractFrom(cookieStringsSeparatedByCommas: String): Result[CookieSet] = {
      val cookieStrings = (List[String]() /: (cookieStringsSeparatedByCommas split ", ")) {
        case (Nil, element) => List(element)
        case (head :: tail, element) => if (element.matches("[0123].+")) s"$head, $element" :: tail
        else element :: head :: tail
      } filter (_.nonEmpty)

      val parsed: Seq[Result[Cookie]] = cookieStrings.reverse map {
        cookieString =>
          parseAll(cookieExp, cookieString) match {
            case Success(result, _) => Good(result)
            case NoSuccess(x, y) =>
              val at = y.offset
              val src = y.source.toString
              val context = src.slice(math.max(0, at - 10), math.max(0, at - 10) + 20)
              Logging.debug(s"BAD COOKIE:\n$cookieString\n=================")
              Result.error(s"Failed to parse: $x in ...$context...")
          }

      }

      val res: Result[Set[Cookie]] = Result.traverse(parsed).map(_.toSet) match {
        case x if x.isEmpty => Good(Set[Cookie]())
        case nonempty: Result[Set[Cookie]] => nonempty
      }

      res map (new CookieSet(_))
    }
  }

  def dateFormat: String => Result[Date] =
    parseFormattedDate("EEE, dd MMM yyyy HH:mm:ss zzz")
}
