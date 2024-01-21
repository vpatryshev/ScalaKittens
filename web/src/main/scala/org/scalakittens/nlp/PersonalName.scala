package org.scalakittens.nlp

import org.scalakittens.Result._

import language.postfixOps
import org.scalakittens.nlp.Linguist._

import scala.util.matching.Regex

/**
 * This class is supposed to store human names, be able to compare them etc.
 */
case class PersonalName(first: String, middle:String, last:String, suffix: Option[String]) { thisName =>
  lazy val firstlast: String = first + " " + last
  lazy val withoutSuffix: String = List(first, middle, last) filter (_.nonEmpty) mkString " "
  private lazy val addSuffix = suffix match {
    case Some(s) if PersonalName.isAbbr(s) => s", $s."
    case Some(s)              => s", $s"
    case _ => ""
  }
  lazy val full: String = withoutSuffix + addSuffix

  def matchWith(name2First: String, name2Middle: String, name2Last: String, name2Suffix:Option[String] = None): Outcome =
  {
    val name2First1 = name2First.replaceAll("-", " ")
    def oneInAnother(s:String, t: String) = (s startsWith t) || (t startsWith s)

    val fullname = name2First1::name2Middle::name2Last::Nil filter (_.nonEmpty) mkString " "
    if (full == fullname) return OK

    val name1Suffix = suffix
    val middleNameChecked: Outcome = OKif(name2Middle.isEmpty || oneInAnother(name2Middle.toLowerCase, middle.toLowerCase),s"problems with middle name: '$name2Middle' vs '$middle'")

    val suffixesMatch = OKif(name1Suffix flatMap (s1 => name2Suffix.map(s1==)) getOrElse true) orCommentTheError "Different suffixes"

    val res = suffixesMatch andThen middleNameChecked andThen {
      val p1First = first.toLowerCase.replaceAll("-", " ")
      val p1Last = last.toLowerCase
      val p2First:String = normalizedName(name2First1).toLowerCase.replaceAll("-", " ")
      val p2Last:String = normalizedName(name2Last).toLowerCase

      val r: Outcome = OKif (withoutSuffix.toLowerCase == name2First1 + name2Middle + name2Last) orElse {
        val firstNamesMatch = p1First.contains(p2First) || p2First.contains(p1First)
        val lastNamesMatch = p1Last.isEmpty || p2Last.isEmpty || (p1Last == p2Last)

        val namesMatch = OKif ((firstNamesMatch && lastNamesMatch) ||
          (firstlast.toLowerCase == p2First + " " + p2Last) ||
          (p1First.isEmpty && (p1Last.endsWith(p2Last) && p1Last.startsWith(p2First)))) orCommentTheError s"Names did not match: fnm=$firstNamesMatch, lnm=$lastNamesMatch, name1=$thisName, name2=='$p2First:$p2Last'"
        namesMatch
      }
      r
    }
    res
  }
}

object PersonalName {

  private val similarNames = Set("richard,dick", "christopher,christoph,christophe") map (_.split(",").toList)

  def listSimilar(name: String): List[String] = {
    val nameToFind = name.split("\\s")(0).toLowerCase
    similarNames.find(_ contains nameToFind) getOrElse List(nameToFind)
  }

  def of(first: String, middle: String, last: String): String = List(first, middle, last).map(normalizedName).filter(!_.isEmpty).mkString(" ")

  private def namePattern = "[\\w '-.,]{2,}"

  def looksValid(s: String): Boolean = s matches namePattern

  private def nameWithRolePattern = "[\\w '-.,]{2,} \\([\\w ]{3,}\\)"

  def canBeNameWithRole(s: String): Boolean = s matches nameWithRolePattern

  val TypicalNameFormat: Regex = "(\\w+)\\b*\\((\\w+)\\)".r
  private val NameWithDob = "([\\w ,.-]+)[ \\(]+(\\d{2}[/.]\\d{2}[/.]\\d{4})\\)?".r
  val roles = Set("self", "you", "subscriber", "member", "wife", "husband", "spouse", "dependent", "child", "son", "daughter")

  val DecoratedNamePattern: Regex = "([^\\(]+)\\(([^\\)]+)\\)".r

  private[nlp] def extractNameString(nameEts: String): String =
    nameEts.trim match {
      case NameWithDob(name, _) => name
      case full@TypicalNameFormat(name, role) => if (roles contains role.toLowerCase) name else full
      case DecoratedNamePattern(name,role) => name.replaceAll("[\\s\u00a0]", " ").trim
      case other => other
    }

  private def buildName(names: Iterable[String]) = names map strippedName mkString " "

  private def splitFirstMiddleNames(source: List[String]): (String, String) = {
    val middles1 = source.reverse takeWhile(_.length == 1) reverse

    if (middles1.nonEmpty) {
      val firsts = source take (source.length - middles1.length)
      val (f,m) = if (firsts.isEmpty && middles1.nonEmpty) {
        (middles1.head::Nil, middles1.tail)
      } else (firsts, middles1)

      (buildName(f), buildName(m))
    } else if (source.length > 1)
      (buildName(source.dropRight(1)), buildName(source.last::Nil))
     else
      (buildName(source), "")
  }

  private val suffixes = Set(" Esq", " Jr", " Sr", " II", " III", " IV", " the 3rd", " the third")
  private[nlp] val isAbbr = Set("Esq", "Jr", "Sr")

  private def suffixOf(name: String): Option[String] = {
    val name0 = name.trim.toLowerCase.replaceAll("\\. *$", "")
    suffixes find (name0 endsWith _.toLowerCase) map (_.trim)
  }

  def strippedName(name: String): String = {
    val name0 = name.trim.toLowerCase.replaceAll("\\. *$", "").replaceAll("' *", "")
    val suffix = suffixOf(name0) getOrElse ""
    val trimmed = name0.substring(0, name0.length - suffix.length)
    toTitleCase(trimmed)(properNameContext)
  }

  private[nlp] def parseName(nameSrc: String): (String, String, String, Option[String]) = {
    val name1 = nameSrc.replaceAll("[.!]", " ").trim
    val name1Suffix:Option[String] = suffixOf(name1)

    val name1NoSuffix = name1Suffix map (s => name1.dropRight(s.length)) getOrElse name1
    val norm = normalizedName(name1NoSuffix)

    val (firstMiddleNames, lastNames) = firstsAndLasts(norm)

    val (firstName, middleName) = splitFirstMiddleNames(firstMiddleNames)

    (strippedName(firstName), strippedName(middleName), strippedName(lastNames mkString " "), name1Suffix)
  }

  private[nlp] def firstsAndLasts(source: String): (List[String], List[String]) = {
    val lastfirst = source.split(",") map (_.trim) filter (_.nonEmpty)
    if (lastfirst.length == 2) {
      (lastfirst.last.split(" ").toList, lastfirst.head :: Nil)
    } else {
      val p0 = source.replaceAll(",", " ").replaceAll(" +", " ").trim.split(" ")

      val splitCandidates = math.max(1, p0.length - 1) :: (properNameContext.lowerCaseWords.map(w => p0 indexOf w) filter (_ > 0) toList)
      val lastNameIndex = splitCandidates min

      val (fs, ls) = p0 splitAt lastNameIndex
      (fs.toList, ls.toList)
    }
  }

  def apply(fullName: String): PersonalName = {
    val goodString = extractNameString(fullName)
    val (f,m,l,s) = parseName(goodString)
    PersonalName(f,m,l,s)
  }
}