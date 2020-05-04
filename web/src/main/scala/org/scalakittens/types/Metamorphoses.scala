package org.scalakittens.types

import java.io.InputStream
import java.util.Date

import org.joda.time.DateTime
import org.scalakittens._
import org.squeryl.customtypes.BigDecimalField

import scala.language.{implicitConversions, postfixOps}

/**
 * All general purpose implicit transformations should be stored here
 */
trait Metamorphoses extends PropsOps with Percentage with Money {
  implicit def success(result: ⇒ Any = true): ResultType.Value = {
    val r = result
    r match {
      case resultValue: ResultType.Value ⇒ resultValue
      case trueOrFalse: Boolean          ⇒ ResultType.apply(trueOrFalse)
      case bad:Bad[_] if !bad.isEmpty    ⇒ ResultType.Error
      case _                             ⇒ ResultType.Success
    }
  }

  implicit def aka(x:Option[BigDecimalField]): Object {
    def aka(name: String): String
  } = new {
    def aka(name: String): String = x.map(name + "=" + _.value.toString) getOrElse ""
  }

  implicit def byteStream(s: String): InputStream = new java.io.ByteArrayInputStream(s.getBytes("UTF-8"))

  def toInt(x: Any): Result[Int] = try {
    x match {
      case s: String ⇒ Good(s.toInt)
      case d: Double ⇒ Good(d.toInt)
      case l: Long   ⇒ Good(l.toInt)
      case i: Int    ⇒ Good(i)
      case _         ⇒ Result.error[Int](s"Can't convert to int: $x")
    }
  } catch {
    case e: Exception ⇒ Result.exception(e)
  }

  class DataExtractor(props:Props, string2date: String⇒Result[Date]) {
    def apply(name:String): Object {
      def text: String
      def $$$: Result[BigDecimal]
      def %%% : Result[BigDecimalField]
      def asDateTime: Result[DateTime]
      def asRecentDate: Result[Date]
      def requireText: Result[String]
    } = new {
// TODO(vlad): figure out if we can alway use this instead, a plurality of possible keys
//      def requireText: Result[String] = Result(props.oneOf(name.split(",").map(_.trim):_*), s"Missing $name")
      def requireText: Result[String] = props valueOf name
      def text:String = requireText getOrElse ""
      def $$$: Result[BigDecimal] = requireText flatMap dollars

     /**
      * Converts a Numeric string representing a percentage to its
      * normalized value, i.e. value between 0 and 1.0
      * @return A [[Result]] object with [[org.squeryl.customtypes.BigDecimalField]] object
      */
      def %%% : Result[BigDecimalField] = requireText flatMap parseAsNormalizedBigDecimal map (new BigDecimalField(_))
      def asRecentDate: Result[Date] = requireText flatMap string2date filter {d ⇒ DateAndTime.isRecent(d)}
      def asDateTime: Result[DateTime] = requireText map DateTime.parse
    }
  }

  def intOr(x: Any, defaultValue: Int): Int = toInt(x) getOrElse defaultValue
}

object Metamorphoses extends Metamorphoses
