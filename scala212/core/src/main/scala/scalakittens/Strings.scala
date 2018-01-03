package scalakittens

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStreamReader}
import java.util.Base64
import java.util.zip._

import scala.io.Source
import scala.language.postfixOps
import scala.util.matching.Regex

trait Strings {

  def isEmpty(s: String): Boolean = s == null || s.trim.isEmpty

  def commonPrefix(s1: String, s2: String): String = {
    if (s1.isEmpty || s2.isEmpty || s1(0) != s2(0)) "" else s1.head + commonPrefix(s1.tail, s2.tail)
  }

  def zip(s: String): String = {
    val baos = new ByteArrayOutputStream()
    val zos = new GZIPOutputStream(baos)
    zos.write(s.getBytes("UTF8"))
    zos.close()
    val bytes = baos.toByteArray
    Base64.getEncoder.encodeToString(bytes)
  }

  def unzip(bytes: Array[Byte]): String = {
    val bios = new ByteArrayInputStream(bytes)
    val zis = new GZIPInputStream(bios)
    Source.fromInputStream(zis, "UTF8").mkString("")
  }

  def unzip(base64: String): String = {
    unzip(Base64.getDecoder.decode(base64))
  }
  
  implicit class powerString(s: String) {
    def containsIgnoreCase(other: String): Boolean = s.toUpperCase.contains(other.toUpperCase)

    def |(alt: String): String = if (s.isEmpty) alt else s

    def js: String = {
      require(s != null, "Illegal value: null")
      val s1 = s.trim
      val src = if (s1.startsWith("'") && s1.endsWith("'")) s1.tail.dropRight(1) else s1
      "'" + src.replaceAll("\\\\", "\\\\\\\\").replaceAll("\'", "\\\\'") + "'"
    }

    def decodeHex: Array[Byte] = {
      // credits: Tomoya Yamamoto, https://gist.github.com/tmyymmt/3727124
      s.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
    }

    def quote: String = {
      val r = "\"" + s.replaceAll("\\\"", "\\\\\"").replaceAll("\n", "\\n") + "\""
      r
    }
  }

  def oneOf(cases: String*): String = cases find (!_.isEmpty) getOrElse ""

  // 000123 → 123
  def trimLeftLeading0(s: String): String = s.trim.dropWhile('0' ==).trim

  // (abcd,ab) → (cd)
  def dropPrefix(s: String, prefix: String): String = {
    val leftPos = s.toUpperCase.indexOf(prefix.toUpperCase)
    (if (leftPos >= 0) s.substring(leftPos + prefix.length) else s).trim
  }

  /**
    * Removes leading characters from a string.
    * E.g. dropLeading("abc")("bank of America") == "nk of America"
    *
    * @param chars the characters to remove
    * @return string trimmer
    */
  def dropLeading(chars: String): String => String = (s: String) ⇒ s dropWhile (chars contains _)

  /**
    * I could not figure out how to do multiline match in scala; so had to write this.
    *
    * @param Reg the regex to match
    * @return the group found (works with one group only
    */
  def multilineMatch(Reg: Regex): String ⇒ Option[String] =
    text ⇒ Reg.findFirstIn(text).flatMap({ case Reg(value) ⇒ Some(value)
    case _ ⇒ None
    })

  /**
    * Extracts a value from an arbitrary string; the value is preceded by prefix and ends with new line
    *
    * @param prefix what precedes the value
    * @return the value; if not found, an empty string (missing value, right?)
    */
  def valueOf(prefix: String): String => String = {
    (text: String) ⇒ prefixSearch(prefix)(text).getOrElse("")
  }

  def prefixSearch(prefix: String): String => Option[String]
  = multilineMatch((".*" + prefix.replaceAll("\\.", "\\\\.") + " *([^\\n]*)").r)

  private val DIGITS = "^(\\d+)$".r

  def digits(s: String): Option[String] = s match {
    case DIGITS(value) ⇒ Some(value)
    case _ ⇒ None
  }

  private val accented = Set("aàáâäæãåā", "cçćč", "eèéêëēėę", "iîïíīįì", "lł", "nñń", "oôöòóœøōõ", "sßśš", "uûüùúū", "yÿ", "zžźż", "'’")

  private val accentCharMap = {
    for {entry <- accented
         variant <- entry.tail} yield variant -> entry.head
  } toMap

  def eliminateAccent(c: Char): Char = accentCharMap.getOrElse(c, c)

  def normalize(s: String): String = {
    val s1: String = s.toLowerCase
    val s2: String = s1 map eliminateAccent
    val s3 = s2.replaceAll("[\\'’]s", "")
    val s4 = s3.replaceAll("[^'a-z]+", " ")
    val s5 = s4.replaceFirst("[’']\\s*$", "")
    val s6 = s5.replaceAll("[\\'’]s", "")
    val s7 = s6.replaceAll("[\\'’] ", " ")
    s7
  }

  val isStop = Set(
    "", 
    "a", "about", "add", "adds", "added", "after", "against", "ah", "all", "along", 
      "already", "also", "always", "am", "among", "an", "and", "another", "any", "are", "as", 
      "ask", "asked", "asks", "at", "away",
    "back", "be", "became", "because", "become", "been", "before", "began", "begin", "behind", 
      "being", "beside", "between", "both", "bring", "brings", "brought", "but", "by", 
    "came", "can", "can't", "come", "continue", "continues", "continued", "could", "couldn't",
    "das", "de", "did", "do", "does", "doing", "don't", "done", "down", 
    "each", "en", "enter", "entered", "enters", "even", "every",
    "feel", "feels", "felt", "for", "from",
    "gave", "get", "give", "given", "gives", "go", "goes", "going", "good", "got", "great",
    "had", "has", "have", "having", "he", "her", "here", "here", 
      "herself", "him", "him", "himself", "his", "how",
    "i", "i'll", "i'm", "i've", "if", "in", "into", "is", "it", "its", "je", "just",
    "knew", "know",
    "lay", "left", "less", "let", "like", "little", "long", "look", "looked", "looking", "looks",
    "made", "make", "many", "may", "me", "might", "mine", "more", "most", "much", "must", "my",
    "near", "new", "no", "nor", "not", "nous", "now", 
    "of", "off", "oh", "old", "on", "one", "only", "or", "other", "others", "our", "out", "over", "own",
    "put",
    "quite",
    "s'en", "said", "same", "sat", "saw", "say", "see", "seemed", "seems", "seen", "sent", "several", 
      "shall", "she", "should", "since", "sit", "so", "some", "something", "soon", "speak", "speaks", 
      "spoke", "stand", "stands", "stood", "still", "such", "suddenly",
    "t", "take", "talk", "tell", "tells", "than", "that", "the", "their", "theirs", "them", "then", 
      "there", "these", "they", "this", "those", "though", "thought", "three", "through",
      "to", "together", "told", "too", "took", "toward", "try", "tries", "tried", "turn", "turned",
      "turns", "two",
    "under", "up", "us", "very",
    "was", "we", "well", "went", "were", "what", "when", "where", 
      "whether", "which", "while", "who", "whole", "whom", "why", "will", "with", "without", "won't", "would", "wouldn't",
    "yes", "yet", "you", "your")
}

object Strings extends Strings
