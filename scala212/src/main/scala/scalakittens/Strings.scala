package scalakittens

import scala.language.postfixOps
import scala.util.matching.Regex

trait Strings {

  def isEmpty(s: String) = s == null || s.trim.isEmpty

  def commonPrefix(s1:String, s2:String):String = {
    if (s1.isEmpty || s2.isEmpty || s1(0) != s2(0)) "" else s1.head + commonPrefix(s1.tail, s2.tail)
  }

  implicit class powerString(s: String) {
    def containsIgnoreCase(other: String)   = s.toUpperCase.contains (other.toUpperCase)
    def strictContainsIgnoreCase(t: String) = !t.isEmpty && containsIgnoreCase (t)
    def | (alt: String) = if (s.isEmpty) alt else s
    def js = {
      require(s != null, "Illegal value: null")
      val s1 = s.trim
      val src = if (s1.startsWith("'") && s1.endsWith("'")) s1.tail.dropRight(1) else s1
      "'" + src.replaceAll("\\\\","\\\\\\\\").replaceAll("\'", "\\\\'") + "'"
    }
    def decodeHex: Array[Byte] = { // credits: Tomoya Yamamoto, https://gist.github.com/tmyymmt/3727124
      s.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
    }

    def quote = {
      val r = "\"" + s.replaceAll("\\\"", "\\\\\"").replaceAll("\n", "\\n") + "\""
      r
    }
  }

  def oneOf(cases: String*) = cases find (!_.isEmpty) getOrElse ""

  // Do the short string show up in the long one? (case in-sensitive)
  def isInStrStrict(sLong:String, sShort:String): Boolean = sLong strictContainsIgnoreCase sShort

  // 000123 → 123
  def trimLeftLeading0(s:String): String = s.trim.dropWhile('0'==).trim

  // (abcd,ab) → (cd)
  def dropPrefix(s: String, prefix: String):String =
  {
    val leftPos = s.toUpperCase.indexOf(prefix.toUpperCase)
    (if (leftPos>=0) s.substring(leftPos + prefix.length ) else s).trim
  }


  /**
    * Removes leading characters from a string.
    * E.g. dropLeading("abc")("bank of America") == "nk of America"
    *
    * @param chars the characters to remove
    * @return string trimmer
    */
  def dropLeading(chars: String) = (s: String) ⇒ s dropWhile (chars contains _)

  /**
    * I could not figure out how to do multiline match in scala; so had to write this.
    *
    * @param Reg the regex to match
    * @return the group found (works with one group only
    */
  def multilineMatch(Reg: Regex): String ⇒ Option[String] =
    text ⇒ Reg.findFirstIn(text).flatMap({case Reg(value) ⇒ Some(value)
    case _         ⇒ None
    })

  /**
    * Extracts a value from an arbitrary string; the value is preceded by prefix and ends with new line
    *
    * @param prefix what precedes the value
    * @return the value; if not found, an empty string (missing value, right?)
    */
  def valueOf(prefix: String) = {
    (text: String) ⇒ prefixSearch(prefix)(text).getOrElse("")
  }

  def prefixSearch(prefix: String) = multilineMatch((".*" + prefix.replaceAll("\\.", "\\\\.") + " *([^\\n]*)").r)

  val DIGITS = "^(\\d+)$".r
  def digits(s: String) = s match {
    case DIGITS(value) ⇒ Some(value)
    case _ ⇒ None
  }
}

object Strings extends Strings
