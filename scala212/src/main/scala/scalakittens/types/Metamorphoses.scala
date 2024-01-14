package scalakittens.types

import org.joda.time.DateTime
import scalakittens._
import scalakittens.types.Money._
import scalakittens.types.Percentage._

import java.io.InputStream
import java.util.Date
import scala.languageFeature.implicitConversions

/**
 * All general purpose implicit transformations should be stored here
 */
trait Metamorphoses extends PropsOps {
  implicit def aka(x:Option[BigDecimal]) = new {
    def aka(name: String): String = x.map(name + "=" + _.toString) getOrElse ""
  }

  implicit def byteStream(s: String): InputStream = new java.io.ByteArrayInputStream(s.getBytes("UTF-8"))

  def toInt(x: Any): Result[Int] = try {
    x match {
      case s: String => Good(s.toInt)
      case d: Double => Good(d.toInt)
      case l: Long   => Good(l.toInt)
      case i: Int    => Good(i)
      case _         => Result.error[Int](s"Can't convert to int: $x")
    }
  } catch {
    case e: Exception => Result.exception(e)
  }

  class DataExtractor(props:Props, string2date: String => Result[Date]) {
    def apply(name:String) = new {
// TODO(vlad): figure out if we can always use this instead, a plurality of possible keys
//      def requireText: Result[String] = Result(props.oneOf(name.split(",").map(_.trim):_*), s"Missing $name")
      def requireText: Result[String] = props valueOf name
      def text:String = requireText getOrElse ""
      def $$$: Result[BigDecimal] = requireText flatMap dollars
      def %%% : Result[BigDecimal] = requireText flatMap parseAsNormalizedBigDecimal
      def asRecentDate: Result[Date] = requireText flatMap string2date filter {d => DateAndTime.isRecent(d)}
      def asDateTime: Result[DateTime] = requireText map DateTime.parse
    }
  }

  def intOr(x: Any, defaultValue: Int): Int = toInt(x) getOrElse defaultValue
}

object Metamorphoses extends Metamorphoses
