package org.scalakittens.nlp

import java.text.Normalizer

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}

/**
 * Here we represent complicated English phrases doggy-style, removing synonyms and meaningless words
 * (@see https://en.wikipedia.org/wiki/SHRDLU)
 */
object Linguist {

  val DEBUG_MODE = false
  private def cinmDebug(s: => String): Unit = if (DEBUG_MODE) println(s"--DEBUG; $s")

  private val meaningless = Set("a", "at", "childhood", "did", "from", "in", "is", "it", "my", "name", "of", "on", "the", "was", "what", "which", "you", "your")

  private val synonyms = Set("city, town", "sibling, siblings", "job, work") .map {
    word => word.split(", ").toList
  } .collect {
    case main::tail => tail map (w => w → main)
  } .flatten .toMap withDefault identity

  private val isBadWord = Set("ass") // TODO(vlad): invent more

  def simplifyQuestion(question: String): String = {
    val noPunctuation = question.toLowerCase.replaceAll("[^a-z ]", "")
    val words = noPunctuation split " "
    val meaningfulWords = words filterNot meaningless
    val standardized = meaningfulWords map synonyms
    standardized mkString " "
  }

  def shorten(word:String, size: Int, name:String): (String, String) = {
    val candidate = word.trim take size
    val words = candidate.split("[ ,\\.;]").toList.reverse
    words match {
      case oneWord::Nil => (candidate, if (isBadWord(oneWord.toLowerCase)) s"$name: keeping bad word <<$oneWord>>" else "")
      case last::previous if isBadWord(last.toLowerCase) =>
        val dotPlace = candidate.lastIndexOf(last.last)
        val (left, right) = candidate.splitAt(dotPlace)
        val tail = right.tail.dropWhile('.'==)
        val good = s"$left.$tail"
        (good, s"$name: fixed bad word <<$last>>")

      case otherwise => (candidate, "")
    }
  }

  trait WordContext {
    def lowerCaseWords: Set[String]
    def upperCaseWords: Set[String]
  }

  object generalWordContext extends WordContext {
    val lowerCaseWords = Set("a", "an", "at", "is", "not", "of", "the")
    val upperCaseWords = Set("HSA","HRA","CT","MRI","NICU","N/A","(N/A)")
  }

  object properNameContext extends WordContext {
    val lowerCaseWords = Set("al", "ben", "bin", "de", "del", "der", "di", "el", "la", "las", "los", "von", "van")
    val upperCaseWords: Set[String] = Set[String]()
  }

  private def titleCasePlainWord(s:String, context: WordContext): String = {
    if (s.isEmpty) return s // G.W.Bush, for Ed Wu, for A Nang...
    if (context.upperCaseWords contains s.toUpperCase) return s.toUpperCase
    if (context.lowerCaseWords contains s.toLowerCase) return s.toLowerCase
    s.substring(0,1).toUpperCase + s.substring(1).toLowerCase
  }

  private def titleCaseWord(s:String)(implicit context: WordContext): String = {
    s.split("-").map(w => titleCasePlainWord(w, context)) mkString "-"
  }

  def cleanupProcedureText(s:String):String =
  {
    val ProcWithCPT = "[a-zA-Z]*[0-9]+\\s-\\s(.+)".r
    val trimmed = s.trim
    if (trimmed.isEmpty) return ""
    val plainText = trimmed match {
      case ProcWithCPT(txt) => txt
      case otherText => otherText
    }

    //   val DescriptionPattern = "(Not Available )?(.*)".r

    val cleanedUpText = plainText /*match {
      case DescriptionPattern(_, d) => d
      case _ => s
    }*/
  val text1 = toTitleCase( cleanedUpText.replaceAll("^[\\W&&[^(]]*","").replaceAll("[\\W&&[^)]]*$","") ).trim
    val text = text1.replaceFirst(" \\(\\d\\)", "")
    text
  }

  /// Converts String to Title Case - TODO rewrite using a single pass, so "MRI/Extremity" works too
  def toTitleCase(s:String)(implicit context: WordContext = generalWordContext):String =
  {
    val s1 = s.trim
    if (s1.isEmpty) return ""
    val theName = s1.replaceAll(",\\s*",", ")
    val text = theName.split("\\s+").map(_.split("\\.").map(titleCaseWord(_)(context)).mkString(".")).mkString(" ")
    if (text.isEmpty) "" else text.substring(0,1).toUpperCase + text.tail
  }

  /**
   * Normalizes the string to a canonical format, and replaces all diacritical marks
    *
    * @param name the name that we are normalizing, just a string
   * @return
   */
  def normalizedName(name: String): String = normalize(PersonalName.strippedName(name))


  /**
   * Normalizes a string to a canonical format, and replaces all diacritical marks
    *
    * @param s the name that we are normalizing, just a string
   * @return
   */
  def normalize(s: String): String = {
//    cinmDebug(s + "→" + s.getBytes("UTF16").toList.grouped(2).map(l => (l(0).toInt & 0xff) << 8 | (l(1).toInt&0xff)).map(_.toHexString).toList)

    Normalizer.normalize(s, Normalizer.Form.NFD).replaceAll("[↑\\p{InCombiningDiacriticalMarks}]+", "").trim
  }

  // When given two strings, will calculate match 'distance' between them. Aka: how similar they are.
  def levenshteinDistance(s : String, t : String) : Int = {

    if (s.isEmpty) t.length
    else if (t.isEmpty) s.length
    else {
      val equal = if (s.head == t.head) 0 else 1
      val d =  math.min(levenshteinDistance(s.tail, t.tail) + equal,
           1 + math.min(levenshteinDistance(s.tail, t),
                        levenshteinDistance(s, t.tail))
           )
      d
    }
  }

  /*
    The following should say "true":

    CheckIfNameMatches("Christopher T Young","Christopher-T","young       ")
    CheckIfNameMatches("Christopher T Young"," Christopher T ","  young ")
    CheckIfNameMatches("Christopher-Lee Young","Christopher-L","young")
    CheckIfNameMatches("Christopher-Lee Young","Christopher M","young")
    CheckIfNameMatches("Christopher Lee Young","Christopher","Lee-young")
  */


}
