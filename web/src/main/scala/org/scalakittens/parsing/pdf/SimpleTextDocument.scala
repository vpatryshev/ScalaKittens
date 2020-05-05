package org.scalakittens.web.common.parsing.pdf

import org.scalakittens.types.Money._
import org.scalakittens.Ops._
import org.scalakittens.types.Types._
import org.scalakittens.web.common._
import org.scalakittens.web.common.parsing.pdf.SimpleTextDocument._
import org.scalakittens.{Props, Result}

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.parsing.json.JSON
import scala.xml.{Elem, Node, NodeSeq}

/**
 * A document consists of pages
  *
  * @param pages - the contents
 */
class SimpleTextDocument(documentBuilder: DocumentBuilder, val pages:List[Page], val name: String = "") {
  def select(predicate: Page ⇒ Boolean) = new SimpleTextDocument(documentBuilder, pages filter predicate, name)
//  def takeWhilePages(predicate: Page ⇒ Boolean) = new SimpleTextDocument(pages takeWhile predicate, name)
  def takeWhile(predicate: Line ⇒ Boolean) = {
    val goodOnes = pages takeWhile (_.contents.forall(predicate))
    val badOne:Option[Page] = pages.find(!_.contents.forall(predicate))

    val lastPageOpt = for (page <- badOne) yield {
        val lines: Seq[Line] = page.contents takeWhile predicate
      new Page(documentBuilder, page.titleOpt, lines.map(_.chunks))
    }

    new SimpleTextDocument(documentBuilder, goodOnes ++ lastPageOpt.toList, name)
  }

  def contains(text: String) = lineContaining(text) isGood

  def isEmpty = pages.isEmpty
  def nonEmpty = pages.nonEmpty

  def findLine(predicate: Line ⇒ Boolean, onError: String = "not found"):Result[Line] = {
    val findings = pages map (p ⇒ p findLine predicate)
    val foundList = findings collect {case Good(x) ⇒ x}
    val r = Result(foundList.headOption, onError)
    r
  }

  def findLines(predicate: Line ⇒ Boolean): List[Line] = {
    val lines: List[Line] = for {
      page <- pages
      line <- page findLines predicate
    } yield line
    lines
  }

  def lineContaining(text: String) = findLine (lineContains(text))

  def linesContaining(text: String) = findLines (lineContains(text))

  def apply(page: Int) = pages(page)
  def head = Result(pages.headOption)
  def apply(key: String): Result[Page] = {
    val found = pages.filter(_.title.matches(key))
    found match {
      case Nil ⇒ Empty
      case p::Nil ⇒ Result.forValue(p)
      case many ⇒ Result.error(s"Found ${many.length} pages matching $key")
    }
  }

  def extractProps = pages map (_.toProps)

  def named(newName: String) = new SimpleTextDocument(documentBuilder, pages, newName)

  override def toString = s"======$name=====\n" + (pages.zipWithIndex map (p ⇒ "\n    PAGE #" + (p._2+1) + "\n" + p._1) mkString "\n")
  def toFormattedString = s"======$name=====\n" + (pages.zipWithIndex map (p ⇒ "\n    PAGE #" + (p._2+1) + "\n" + p._1.toFormattedString) mkString "\n")

  def toSource = s"""{"$name", "content"=[""" + (pages.zipWithIndex map (p ⇒ p._1.toSource) mkString "\n]}")
}

trait SimpleTextDocumentOps {
  val defaultProximity = 6
  val maxProximity = 20

  def parse(src: String): Result[SimpleTextDocument] = Page.parse(src) map (page ⇒ new SimpleTextDocument(page.documentBuilder, page::Nil))
}

object SimpleTextDocument extends SimpleTextDocumentOps {

  private val KVpattern = "(.*):(.*)".r
  private val Kpattern = "\\s*([^:]+)\\s*:\\s*".r
  private val Vpattern = "\\s*([^:]+)\\s*".r

  def And(p1: Line ⇒ Boolean, p2: Line ⇒ Boolean) = (line:Line) ⇒ p1(line) && p2(line)
  def Or(p1: Line ⇒ Boolean, p2: Line ⇒ Boolean) = (line:Line) ⇒ p1(line) || p2(line)

  def lineContains(text: String) = (line:Line) ⇒ line has (_ contains text)

  def lineDoesNotContain(text: String) = (line:Line) ⇒ !lineContains(text)(line)
  def lineMatches(regex: String) = (line:Line) ⇒ line has (_ matches regex)

  def intersect(range1: (Int, Int)) = (range2: (Int, Int)) ⇒ {
    val ok = (range1._1+range1._2 >= range2._1) && (range1._1 < range2._1+range2._2)
    ok
  }

  def toInt(whatever: Any) = {
    (""+whatever).split("\\.", 2)(0).toInt
  }

  case class DocumentBuilder(proximity: Int) { self ⇒

    def buildLine(number: Int, strings: Seq[String]): Line = Line(self, number, chunks(strings))
    val EmptyLine = Line(self, -1, Nil:List[Chunk])

    def buildDocument(html: Elem, splitter: String ⇒ Seq[String] = s ⇒ Array(s)) = {
      // retrieve only lowest level divs
      def isDiv(node: Node): Boolean = node.label == "div"

      def containsDiv(node: Node): Boolean =
        node.child.exists(subNode ⇒ isDiv(subNode) || containsDiv(subNode))

      val pageSources:NodeSeq = html \\ "div" filter (!containsDiv(_))
      val pages = pageSources .map (page(_, splitter)) .toList
      new SimpleTextDocument(self, pages)
    }

    def chunks(line: Iterable[String]): List[Chunk] = ((0, Nil:List[Chunk]) /: line)({case ((n, list), s0) ⇒
      val s = s0.trim
      val leadingSpaces = s0 indexOf s
      (n+s0.length, Chunk(s, n + leadingSpaces, s.length)::list)
    })._2.reverse

    def page(node: Node, split: String ⇒ Seq[String]) = {
      val lines: Seq[String] = node \ "p" map (_.text)
      val splitLines: Seq[Seq[String]] = (lines map split).toList
      val chunksOnThisPage = splitLines map chunks
      new Page(self, None, chunksOnThisPage)
    }

    def newOne(m: Map[_,_]): Result[Chunk] = {
      val chunkProps = props(m)
      val chunkSource:Result[(String, (String, String))] = (chunkProps @@ "text") andAlso (chunkProps @@ "pos" andAlso (chunkProps @@ "width"))
      for ((text, (pos, width)) <- chunkSource) yield
      Chunk(text, toInt(pos), toInt(width))
    }
  }

  object DefaultDocumentBuilder extends DocumentBuilder(defaultProximity)

  /**
   * A line of text consists of fragments
    *
    * @param chunks list of chunks
   */
  case class Line(documentBuilder: DocumentBuilder, number: Int, chunks: List[Chunk]) extends ArrayOf[String] {
    def buildLine(i: Int, strings: List[String]) = documentBuilder.buildLine(i, strings)

    def newOne(t: String, p: Int, w: Int): Chunk = new Chunk {
      val text = t
      val position = p
      val width = w
    }

    def merge(first:Chunk, second: Chunk, proximity: Int): Chunk = {
      newOne(first.text + " " + second.text, first.position, second.position + second.width - first.position + proximity)
    }

    def isAdjacent(proximity: Int)(first: Chunk, second: Chunk) = first.position + proximity + first.width >= second.position && first.position <= second.position

    def mergedChunks(proximity: Int, chunks: Seq[Chunk]): List[Chunk] = {
      val sorted = chunks.sortBy(_.position).toList
      val merged = ((Nil:List[Chunk])/: sorted){
        case(Nil, c) ⇒ c::Nil
        case(c1::tail, c2) ⇒
          if (isAdjacent(proximity)(c1, c2)) merge(c1, c2, proximity)::tail else c2::c1::tail
      }
      merged.reverse
    }

    def mergeChunks(proximity: Int) = Line(documentBuilder, number, mergedChunks(proximity, chunks))

    lazy val contents: List[String] = chunks map (_.text.trim)

    private def splitChunksAt(proximity: Int): List[(Option[String], Option[String])] = {
      val myChunks: List[Chunk] = mergedChunks(defaultProximity, chunks) // TODO(vlad): get rid of default proximity here
      val splitChunks: List[(Option[String], Option[String])] =
        myChunks map (_.text) collect {
          case KVpattern(k, v) ⇒ Some(k) -> Some(v).filter(_.nonEmpty)
          case Kpattern(k) ⇒ Some(k) -> None
          case Vpattern(v) ⇒ None -> Some(v)
        }

      splitChunks
    }

    private def extractProps(tentativeKeysAndValues: List[(Option[String], Option[String])] ): Props = {
      val kvPairs = ((List[(String,String)](),None:Option[String])/:tentativeKeysAndValues)((collected, pair) ⇒
        pair match {
          case (Some(key), Some(value)) ⇒ ((key, value.trim):: collected._1, None)
          case (Some(key), None)        ⇒ (collected._1, Some(key))
          case (None, Some(value))      ⇒
            collected._2 match {
              case Some(key) ⇒ ((key, value.trim):: collected._1, None)
              case None      ⇒ (collected._1, None)
            }
          case (None, None) ⇒ collected
        }
      )

      Props(kvPairs._1.toMap)
    }

    def props: Props = propsAtProximity(defaultProximity)

    private def propsAtProximity(proximity: Int) = extractProps(splitChunksAt(proximity))

    def findPropsHaving(keys: Set[String]): Props = {
      val allVariants:Seq[Props] = for (proximity <- 1 to maxProximity)
        yield propsAtProximity(proximity).filterKeys(keys)

      val found = Props.accumulate(allVariants)
      found
    }

    lazy val size = contents.size
    def drop(n: Int) = Line(documentBuilder, -1, chunks drop n)
    def slice(start: Int, end: Int) = Line(documentBuilder, -1, chunks.slice(start, end))
    def ++(other: Line) = Line(documentBuilder, -1, chunks ++ other.chunks)
    def contains(what: String) = {
      contents mkString "" replaceAll(" ", "") contains what.replaceAll("[ \\|]", "")
    }

    def findDollars: Result[BigDecimal] = {
      val allDollars = contents map dollars filter (_.isGood) toList

      allDollars match {
        case goodOne::Nil ⇒ goodOne
        case Nil ⇒ Result.error("No dollars found in this line")
        case one::two::_ ⇒ Result.error("Found more than one dollar amount in this line; can't decide")
      }
    }

    override def toString = "[" + number + "] " + (contents mkString "|")
    private val Scale = 32

    def tabulate(scale: Int):(Int, String) = ((0, "") /: chunks)((current,chunk) ⇒ {
      val (pos, s) = current

      val chunkPos = math.min(pos, chunk.position/scale)
      val shim = " " * math.max(0, chunkPos)
      (chunkPos + chunk.width/scale, s + shim + chunk.text)
    })

    lazy val tabulated = tabulate(Scale)._2

    lazy val toFormattedString = "[%02d] ".format(number) + tabulated

    lazy val toSource = s"{line=$number, chunks=[${chunks map (_.toSource) mkString ","}]}"
  }

  /**
   * A page of text consists of lines
    *
    * @param lineSources a sequence of strings
   */
  case class Page(documentBuilder: DocumentBuilder, titleOpt: Option[String] = None, lineSources: Seq[List[Chunk]]) extends ArrayOf[Line] {

    def contains(s: String): Boolean = {
      findLine (_ contains s) isGood
    }

    def mergedChunks(proximity: Int) = Page(documentBuilder, titleOpt, contents map (_.mergeChunks(proximity).chunks.toList))

    def withTitle(newTitle: String) = Page(documentBuilder, Some(newTitle), lineSources)

    def linesBetween(from: Int,  to: Int ): Seq[Line] = contents.drop(from).take(to-from+1)
    def linesBetween(from: Line, to: Line): Seq[Line] = linesBetween(from.number, to.number)

    val contents: List[Line] = ((lineSources zipWithIndex) map ((p: (Seq[Chunk], Int)) ⇒ Line(documentBuilder, p._2, p._1.toList))).toList
    val size = contents.size

    def indexOfLineContaining(text:String) = this where lineContains(text) orCommentTheError s"Failed to find <<$text>> in this page ${titleOpt getOrElse ""} of $size lines."

    def findLine(predicate: Line ⇒ Boolean):Result[Line] = this where predicate map this.apply

    def findLines(predicate: Line ⇒ Boolean):List[Line] = contents filter predicate

    private def propWithPrefixes(k:String,v:String,prefix: String) = (prefix+"."+k) -> v.trim

    @tailrec
    private def appendPropsFromTable(strings: List[String], prefixes: List[String], collected: Props): (Props, List[String]) = {
      strings match {
        case KVpattern(k, v)::tail ⇒
          val newProp = Map((k::prefixes).reverse.mkString(".") -> v.trim)
          val newProps = collected ++ newProp
          appendPropsFromTable(tail, prefixes, newProps)
        case _              ⇒ (collected, strings)
      }
    }

    private def stripOneKey(k:String) = k.replaceAll("[\\.,\\(\\)]", " ").trim()

    private def parsePropsFromTable(strings: List[String], prefixes: List[String], collected: Props): (Props, List[String]) = {
      strings match {
        case Nil ⇒ (collected, Nil)
        case KVpattern(k, v)::tail ⇒ appendPropsFromTable(strings, prefixes, collected)
        case s::tail ⇒
          val newPrefixes = s.split(" ").toList reverseMap stripOneKey
          val (newProps, newTail) = parsePropsFromTable(tail, newPrefixes:::prefixes, collected)
          parsePropsFromTable(newTail, prefixes, newProps)
      }
    }

    private def parsePropsFromText(strings: List[String]):Props = {
      val groups = groupPrefix(strings)(_ contains ":").map(_ mkString "\n").filter(_ contains ":")
      val map: List[(String, String)] = groups.map(_.split(":", 2)).map(list ⇒ (list(0).trim.toLowerCase, list(1).trim))
      props(map toMap)
    }

    def toProps: Props = {
      val strings = contents.map(_.contents.mkString("").trim)
      strings match {
        case Nil ⇒ Props.empty
        case KVpattern(k, v)::tail ⇒ parsePropsFromText(strings)
        case prefix::tail   ⇒ parsePropsFromTable(strings, Nil, Props.empty)._1
      }
    }

    def findSomeProps(requiredKeys: Set[String]): Props = {
      val fromAllLines = for {
        line <- contents
      } yield line.findPropsHaving(requiredKeys)

      val found = Props.accumulate(fromAllLines)
      found
    }

    protected def parseTableBetweenLines(line0: Line, lineN: Line, headerHeight: Int, proximity: Int): Result[Seq[Props]] =
      parseTableBetweenLines(line0.number + 1, lineN.number - 1, headerHeight, proximity)

    private def indexMissingAttributes(props:Seq[Props], required: Set[String]): Seq[(Int, Props, Set[String])] =
      props.zipWithIndex map {
        case (item, i) ⇒
          (i, item, required -- item.keySet)
      }

    private type ItemRow = (Int, Props, Set[String])

    private def pairsOfRows(data: Seq[ItemRow]): List[(ItemRow, ItemRow)] = {
      data.grouped(2) map (_.toList) collect {
        case firstRow::secondRow::Nil ⇒ (firstRow, secondRow)
      } toList
    }


    private def haveTwo_RowItems(data: Seq[ItemRow]): Boolean = {
      data.length %2 == 0 && {
        val pairs = pairsOfRows(data)
        val so = pairs forall {
          case (firstRow, secondRow) ⇒
            val ok = secondRow._3.nonEmpty && secondRow._3.forall(firstRow._2.keySet.contains)
            ok
        }

        so
      }
    }

    def concatProperties(p1:Props, p2:Props) = {
      val intersection:Set[String] = p1.keySet.intersect(p2.keySet)
      val newMap:Map[String, String] = intersection.map(k ⇒ k -> (p1(k) + " " + p2(k))).toMap

      val concatenatedProps = Props.fromMap(newMap)
      p1 ++ p2 ++ concatenatedProps
    }

    def checkPropertiesSet(requiredKeys: Set[String]) = (props: Seq[Props]) ⇒ {
      val indexedData = indexMissingAttributes(props, requiredKeys)
      val itemProps = if (haveTwo_RowItems(indexedData)) {
        val newProps = pairsOfRows(indexedData) map { case (r1, r2) ⇒ concatProperties(r1._2, r2._2)}
        newProps
      } else props

      val problems =
        if (props.isEmpty) "No item lines found" else {
          indexMissingAttributes(itemProps, requiredKeys) collect {
            case (i, item, missing) if missing.nonEmpty ⇒ (i, s"Missing: $missing in $item")
          } mkString "\n"
        }
      if (problems.isEmpty) Good(itemProps) else Result.error(problems)
    }

    def parseTableHavingKeys(requiredKeys: Set[String])(line0: Int, lineN: Int, headerHeight: Int): Result[Seq[Props]] = {
      if (requiredKeys.isEmpty) return Result.error("No keys to check")
      val checker = checkPropertiesSet(requiredKeys)
      for (i <- 1 to maxProximity) {
        val proximity = i
        val res = parseTableBetweenLines(line0, lineN, headerHeight, proximity)
        val checked = res flatMap checker

        if (checked.isGood) {
          return checked
        }
      }

      val finalVersion = parseTableBetweenLines(line0, lineN, headerHeight, defaultProximity)
      val checkedFinalVersion = finalVersion flatMap checker orCommentTheError "Failed to find good proximity"
      checkedFinalVersion
    }

    private def mergeTwoRangeLists(r1: List[(Int, Int)], r2: List[(Int, Int)]) = {
      val result = r1 map {
        range ⇒ {
          val foundOpt = r2.find(intersect(range))
          foundOpt match {
            case Some(found:(Int, Int)) ⇒
              val x0 = math.min(range._1, found._1)
              val x1 = math.max(range._1+range._2, found._1+found._2)
              (x0, x1-x0)
            case notFound ⇒ range
          }
        }
      }
      result
    }

    private def mergeRanges(rangeList: List[List[(Int, Int)]]): List[(Int, Int)] = rangeList match {
      case Nil ⇒ Nil
      case oneRow::Nil ⇒ oneRow
      case head::tail ⇒ mergeTwoRangeLists(mergeRanges(tail), head)
    }

    private def join(strings: List[Option[String]]): String = strings collect { case Some(s) ⇒ s.trim} filter (_.nonEmpty) mkString " "

    private def tabulateHeaders(columnRanges: List[(Int, Int)])(line: Line): Map[(Int, Int), String] = {
      val order = new Ordering[(Int, Chunk)]{ def compare(p1: (Int, Chunk), p2: (Int, Chunk)) = p1._1.compare(p2._1)}
      val chunksFound = columnRanges map (range ⇒ range -> {
        val whatFound = line.chunks.filter(chunk ⇒ {
          val what = chunk.overlapsWith(range)
          what
        }
        )
        if (whatFound.isEmpty) None else {
          val ctr = range._1+range._2/2
          val withDistances = whatFound map (chunk ⇒ (math.abs(chunk.center - ctr), chunk))
          val ourGuy = withDistances.min(order)
          Some(ourGuy._2)
        }
      })
      val goodChunks = chunksFound collect { case (pos, Some(chunk)) ⇒ pos -> chunk.text}
      goodChunks toMap
    }

    private def findNearestRange(columnRanges: List[(Int, Int)])(c:Chunk) = {
      val middle = c.center
      def distance(x:Int, range:(Int, Int)) = {
        val left = range._1
        val right = range._1+range._2
        if (x < left) left - x else
        if (x > right) x - right else 0

      }

      val rangesByDistance = columnRanges.toList.sortBy(r ⇒
        distance(middle, r)
      )

      rangesByDistance.head
    }

    private def tabulateData(columnRanges: List[(Int, Int)])(line: Line): Map[(Int, Int), String] = {
      val chunkToRange:List[(Chunk, (Int, Int))] = line.chunks map (c ⇒ c -> findNearestRange(columnRanges)(c))

      val grouped: Map[(Int, Int), List[(Chunk, (Int, Int))]] = chunkToRange.groupBy(_._2)

      val range2chunks: Map[(Int, Int), List[Chunk]] = grouped mapValues { list ⇒ list.map(_._1) }

      val range2chunk = range2chunks mapValues {
        (chunks: List[Chunk]) ⇒ {
          val text = chunks map (_.text) mkString " "
          text
        }
      }

      range2chunk toMap
    }

    private[parsing] def parseTableBetweenLines(line0: Int, lineN: Int, headerHeight: Int, proximity: Int = defaultProximity): Result[Seq[Props]] = {
      val rowRange = linesBetween(line0, lineN)
      val headerRows = rowRange take headerHeight map (_.mergeChunks(proximity))
      val dataRows = rowRange drop headerHeight
      if (headerRows.length < headerHeight) {
        Result.error(s"Not enough header rows between $line0 and $lineN")
      } else if (dataRows.isEmpty) {
        Result.error(s"Parsing text document; no rows found between $line0 and $lineN")
      } else {

        val tableOfColumnRanges:List[List[(Int, Int)]] = headerRows.map (_.chunks map (c ⇒ (c.position, c.width)) toList) toList

        val mergedRanges = mergeRanges(tableOfColumnRanges.reverse)

        val neighborRanges = mergedRanges match {
          case Nil ⇒ Nil
          case head::tail ⇒ mergedRanges zip (tail ++ List((10000, 0)))
        }

        val columnRanges: List[(Int, Int)] = neighborRanges map { case ((x0, w0), (x1, _)) ⇒ (x0, math.max(w0, x1 - x0 - proximity)) }

        val tabulatedHeaders = headerRows map tabulateHeaders(columnRanges)

        def headersAt(range: (Int, Int)) = join(tabulatedHeaders map (_.get(range)) toList)
        val headers0: Map[(Int, Int), String] = columnRanges map (range ⇒ range -> headersAt(range)) toMap

        val WithNote = "(.*)\\*\\)? *".r
        val headers = headers0 mapValues {
          case WithNote(v) ⇒ v.trim
          case v           ⇒ v.trim
        }

        val dataIndexed = dataRows map tabulateData(columnRanges)
        val dataWithNames: Seq[Map[String, String]] = dataIndexed map (
          rowMap ⇒ rowMap.keys map (pos ⇒ headers(pos) -> rowMap(pos)) toMap
          )

        val dataAsProps: Seq[Props] = dataWithNames map Props.apply
        Good(dataAsProps)
      }
    }

    def title = titleOpt getOrElse ""
    override def toString = "<<" + title + ">>\n" + (contents mkString "\n") + "\n--END-OF-PAGE--\n"
    lazy val toFormattedString = "<<" + title + ">>\n" + (contents map (_.toFormattedString) mkString "\n") + "\n--END-OF-PAGE--\n"

    private val formatChunks = (chunks: Seq[Chunk]) ⇒ chunks map (_.toSource) mkString ("\n[", ", ", "]")

    def dollarsOnLineHaving(tag: String): Result[BigDecimal] =
      for {line      <- findLine(_ contains tag)
           amount    <- line.findDollars
      } yield amount

    def positiveDollarsOnLineHaving(tag: String): Result[BigDecimal] =
      for (amt <- dollarsOnLineHaving(tag)) yield
        if (amt < 0) -amt else amt

    lazy val toSource = "{" + titleOpt.map(txt ⇒ s""""title":"$txt", """).getOrElse("") + "\"content\":" + (lineSources map formatChunks mkString("\n[", ", ", "]")) + "}"
  }

  object Page {
    val empty = Page(DefaultDocumentBuilder, None, Nil)

    private val LineFormat = "\\[(\\d+)\\] (.*)".r

    def parse(s: String): Result[Page] = {
      val lines = s.split("\\n").toList collect { case LineFormat(lineno, text) ⇒
        text.split("\\|").toList
      }

      Good(Page(DefaultDocumentBuilder, None, lines map DefaultDocumentBuilder.chunks))
    }

    val fromJSON: (String ⇒ Result[Page]) = (source: String) ⇒ {
      val tsup = for {
        json <- Result(JSON.parseFull(source), s"weird json: $source")
        r <- buildPage(json)
      } yield r
        tsup
      }

    def buildPage(json: Any): Result[Page] = {
      json match {
        case top: Map[_, _] ⇒
          val map = top map { case (k, v) ⇒ k.toString -> v}
          val titleOpt = map.get("title") map (_.toString)
          val content = map.getOrElse("content", Nil)
          content match {
            case list: List[_] ⇒
              val rowsProbably = list collect {
                case row: List[_] ⇒
                  val chunksProbably = row.collect {
                    case m: Map[_, _] ⇒ DefaultDocumentBuilder.newOne(m)
                  }

                  val rowProbably = Result.traverse(chunksProbably) map (_.toList)

                  rowProbably
              }

              val pageOpt = Result.traverse(rowsProbably) map (rows ⇒ Page(DefaultDocumentBuilder, titleOpt, rows))
              pageOpt
            case bad ⇒ Result.error(s"Bad json, no content: $content")
          }
        case basura ⇒ Result.error(s"Bad json, no doc here: $json")
      }
    }
  }

  val fromJSON: (String ⇒ Result[SimpleTextDocument]) = (source: String) ⇒ {
    val tsup = for {
      json <- Result(JSON.parseFull(source), s"weird json: $source")
      r <- buildDoc(json)
    } yield r
    tsup
  }

  def buildDoc(json: Any): Result[SimpleTextDocument] = {
    json match {
      case list: List[_] ⇒
        val pagesProbably = list map Page.buildPage
        val pages = Result.traverse(pagesProbably)
        val docOpt: Result[SimpleTextDocument] = pages map
                (pp ⇒ new SimpleTextDocument(DefaultDocumentBuilder, pp))
        docOpt
      case basura ⇒ Result.error(s"Bad json, no doc here: $json")
    }
  }
}


