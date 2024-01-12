package org.scalakittens

import org.scalakittens.Ops.tryOr

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}
import scala.util.matching.Regex

trait Strings {

  val HasDigits: Regex = "\\d".r

  def isEmpty(s: String): Boolean = s == null || s.trim.isEmpty

  private def cp(s1:String, s2:String):String = if (s1.isEmpty || s2.isEmpty || s1(0) != s2(0)) "" else s1.head + cp(s1.tail, s2.tail)

  def commonPrefix(s1:String, s2:String):String = {
    val cp1 = cp(s1, s2)
    cp1
  }
  
  implicit def powerString(s: String): Object {
    def strictContainsIgnoreCase(t: String): Boolean

    def |(alt: String): String

    def quote: String

    def js: String

    def decodeHex: Array[Byte]

    def containsIgnoreCase(other: String): Boolean
  } = new {
    def containsIgnoreCase(other: String): Boolean = s.toUpperCase.contains (other.toUpperCase)
    def strictContainsIgnoreCase(t: String): Boolean = !t.isEmpty && containsIgnoreCase (t)
    def | (alt: String): String = if (s.isEmpty) alt else s
    def js: String = {
      require(s != null, "Illegal value: null")
      val s1 = s.trim
      val src = if (s1.startsWith("'") && s1.endsWith("'")) s1.tail.dropRight(1) else s1
      "'" + src.replaceAll("\\\\","\\\\\\\\").replaceAll("\'", "\\\\'") + "'"
    }
    def decodeHex: Array[Byte] = { // credits: Tomoya Yamamoto, https://gist.github.com/tmyymmt/3727124
      s.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
    }

    def quote: String = {
      val r = "\"" + s.replaceAll("\\\"", "\\\\\"").replaceAll("\n", "\\n") + "\""
      r
    }
  }



  def oneOf(cases: String*): String = cases find (!_.isEmpty) getOrElse ""

  // Do the short string show up in the long one? (case in-sensitive)
  def isInStrStrict(sLong:String, sShort:String): Boolean = sLong strictContainsIgnoreCase sShort

  // Removes starting chars that are not [A-Za-z...] (unicode letters)
  def TrimLeftToAlpha(s:String):String =
  {
    val pattern = "(\\p{L}+)".r
    val matcher  = pattern.pattern.matcher(s)
    if (!matcher.find()) return s.trim // there's no text in it, just return
    s.substring(matcher.start).trim
  }

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
   * @param chars the characters to remove
   * @return string trimmer
   */
  def dropLeading(chars: String): String => String = (s: String) => s dropWhile (chars contains _)

  /**
   * I could not figure out how to do multiline match in scala; so had to write this.
   * @param Reg the regex to match
   * @return the group found (works with one group only
   */
  def multilineMatch(Reg: Regex): String => Option[String] =
    text => Reg.findFirstIn(text).flatMap({case Reg(value) => Some(value)
                                            case _         => None
      })

  /**
   * Extracts a value from an arbitrary string; the value is preceded by prefix and ends with new line
   * @param prefix what precedes the value
   * @return the value; if not found, an empty string (missing value, right?)
   */
  def valueOf(prefix: String): String => String = {
    text: String => prefixSearch(prefix)(text).getOrElse("")
  }

  def prefixSearch(prefix: String): String => Option[String] = multilineMatch((".*" + prefix.replaceAll("\\.", "\\\\.") + " *([^\\n]*)").r)

  val DIGITS: Regex = "^(\\d+)$".r
  def digits(s: String): Option[String] = s match {
    case DIGITS(value) => Some(value)
    case _ => None
  }


  def extractInt(s: String, orElse: Int): Int = tryOr(s.toInt, orElse)

  def indexesOf(s: String, p: Char => Boolean): List[Int] = {
    s.zipWithIndex.filter(cn => p(cn._1)).map(_._2).toList
  }

  def indexesWhere(ss: Seq[String], p: String=>Boolean): List[Int] = {
    def iw(from: Int): List[Int] = {
      val found = ss.indexWhere(p, from)
      if (found < 0) Nil else found :: iw(found + 1)
    }
    iw(0)
  }

  def asString(opt: Option[_]): String = opt match {
    case Some(x) => ""+x
    case None    => "==undefined=="
  }

}

object Strings extends Strings
