package org.scalakittens.web.common.parsing

import org.scalakittens.Library._
import org.scalakittens.web.common._
import org.scalakittens.nlp.Linguist
import org.scalakittens.web.common.parsing.Html._
import org.scalakittens.web.common.parsing.HtmlContentExtractor.{FailedToParse, ParsingResult, SuccessfullyParsed}
import org.scalakittens.web.common.parsing.PropBuffer._
import org.scalakittens.{Logging, Props, Result}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.language.{implicitConversions, postfixOps, reflectiveCalls}
import scala.xml._
import scala.xml.parsing.ConstructingParser

trait HtmlContentExtractor {
  import ExtractorOps._

  val PropertyFormat = "(\\w+)\\b*:\\b*(.+)"

  def transformer:(String⇒String) = identity

  /**
    * This function is supposed to be a universal props extractor. Imagine you have a table
    * with a couple of header rows, and columns, of which the first column contains prop names.
    * E.g.
    * Title H1 H1 H2 H3
    *       h1 h2 h3 h4
    * n1    a  b  c  d
    * n2    e  f  g  h
    *
    * numHeaderRows must be 2; and as a result you get
    * Title.H1.h1.n1→a, Title.H1.h1.n2→e, Title.H1.h2.n1→b, Title.H1.h2.n2 → f
    * etc
    * TODO(vlad): include the case where props are inside cells (detect them by ':')
    * All errors are reported in Bad response
    *
    * @param table rectangular table of rows of string
    * @param numHeaderRows how many rows in header
    * @return Props, if lucky
    */
  def extractPropsOutOfHtmlLikeTable(table:Seq[Seq[String]], numHeaderRows: Int = 1): Result[Props] = {
    def stripKey(key: String) = key.trim.split("\\.|:")(0)

    val trimmed = table.map(_.map(_.trim))
    val t1: Seq[Seq[String]] = trimmed.filter(!_.forall(_.isEmpty)) // remove empty rows
    if (numHeaderRows < 0)                return Result.error[Props](s"Wrong number of header rows: $numHeaderRows")
    if (t1.size < numHeaderRows)          return Result.error[Props](s"Not enought rows (${t1.size}), need at least $numHeaderRows")
    if (t1.size == numHeaderRows)         return Empty
    val w = t1.head.size
    val h = t1.size
    if (!t1.forall(_.size == w))      return Result.error(s"This matrix is not rectangular; expected all rows to contain exactly $w cells")
    val t2 = t1.transpose
    val cleanedUp = t2.filter(!_.forall(_.isEmpty)).transpose
    val width = cleanedUp.head.size
    val mainHeaders         = (1 until width) map    {i ⇒ (0 until numHeaderRows) map    (cleanedUp(_:Int)(i)) map stripKey mkString "."}
    val someHeadersAreEmpty = (1 until width) exists (i ⇒ (0 until numHeaderRows) exists (cleanedUp(_: Int)(i).isEmpty))
    if (someHeadersAreEmpty) return Result.error(s"Headers should not have empty components: ${mainHeaders mkString ","}")

    def isSubheaderRow(row: Seq[String]) = row.forall(_.trim.endsWith(":"))
    val contentRows = cleanedUp drop numHeaderRows
    var headers = mainHeaders
    val lol = for (row ← contentRows) yield {
      if (isSubheaderRow(row)) {
        headers = (mainHeaders zip row) map (p ⇒ p._1 + "." + stripKey(p._2))
        IndexedSeq[(String, String)]()
      }
      else (1 until width). map (j ⇒ (headers(j - 1) + "." + stripKey(row.head)) → row(j).trim)
    }
    val propsList = lol.flatten

    var goodProps: Seq[(String, String)] = propsList.filter(!_._2.isEmpty)
    Good(props(goodProps toMap))
  }

  val DataWeDrop = "canvas,center,col,colgroup,cufon,em,font,form,hr,input,link,mwe,tbody,thead" .split(",") .map("</?" + _ + "[^>]*>") ++
    "align,aria-label,aria-labelledby,aria-describedby,aria-disabled,background,border,bt-xtitle,dir,height,nowrap,onclick,role,src,tabindex,target,title,valign,width" .split(",") .map(" "+_+"=\"[^\"]*\"") ++
    List(
      " rel=\\\"[^\\\"]*\\\"",
      "[\uFFF9-\uFFFE]",
      "<script.*?</script>",
      "<style.*?</style>",
      "<a></a>",
      "<div>\\s*</div>", "<div/>",
      " alt=\"\"", "",
      " href=\"[^\"#][^\"]*\"", "",
      "<img\\s*>\\s*</img>", "<img\\s*/>",
      "<a[^>]*>\\s*</a>",
      "<li>\\s*</li>",
      "<style.*?</style>",
      "<table></table>"
    )

  val Spaces = " +,&nbsp;,\u00a0" split ","
  val NewLines = "\\s*\\n+,<br[^>]*>" split ","

  def replaceAll(html:String, segments:Seq[String], withWhat:String) = {
    def replace(source:String, what: String) = source.replaceAll(what, withWhat)
    (html /: segments) (replace)
  }

  def cleanupHtml(html: String) = {
    val tailDropped = if (html contains "</html>") (html.split("</html>")(0).trim + "</html>") else html
    val html0 = ("<body>" + tailDropped.replaceAll("<!DOCTYPE[^>]*>","") + "</body>").
      replaceAll("</?col [^/>]*/?>", "").
      replaceAll("</?col/?>", "").
      replaceAll("</?colgroup/?>", "").
      replaceAll("</tbody>\\s*<thead","</tbody></table>\n<table><thead").
      replaceAll("</tbody>\\s*<tbody","</tbody></table>\n<table><tbody").
      replaceAll("<body>\\s*<tbody","<body><table><tbody").
      replaceAll("</tbody>\\s*</body>","</tbody></table></body>").
      replaceAll("<tbody[^>]*>\\s*<td","<tbody><tr><td").replaceAll("</td>\\s*</tbody", "</td></tr></tbody").
      replaceAll("(?i)<meta([^>]*)>", "<meta$1/>").
      replaceAll("<img([^>]*)>", "<img$1/>").
      replaceAll("(?i)<img[^>]*alt=\"([^\"]+)\"[^>]*>", "<altimg \"$1\">").
      replaceAll("(?i)<img[^>]*alt=\'([^\']+)\'[^>]*>", "<altimg \"$1\">").
      replaceAll("(?i)<img([^>]*)>", "").
      replaceAll("(?i)<altimg \"([^\"]+)\">", "<img alt=\"$1\"/>").
      replaceAll("//>", "/>").
      replaceAll("<span ([^/>]*)/></span>", "<span $1></span>").
      replaceAll("(?i)(<area[^>]*>)","$1</area>").
      replaceAll(" ng-[\\w]+=\"[^\"]*\"", "").
      replaceAll(" ui-[\\w]+=\"[^\"]*\"", "").
      replaceAll("<hr([^>]*)>","""<hr$1/>""").
      replaceAll(",=\"", "XX=\"")

    def cureMeta(s: String) = s.replaceAll("(?i)<meta([^>]*) ([^ a-zA-Z\\.][^= ]*=[\"\'][^\"\']*[\"\'])", "<meta $1")
    val html1 = Iterator.iterate(html0)(cureMeta).drop(10).next()

    val html2 = replaceAll(html1, DataWeDrop, "")
    val html3 = replaceAll(html2, Spaces, " ")
    val html4 = replaceAll(html3, NewLines, "''''NL''''")

    val html5 = html4.
      replaceAll("<!--.*?-→", "").
      replaceAll(":\\s*<a .*?</a>", ":"). // this means, we have a prefix, and an explanation link; forget the link.
      replaceAll("<table[^>]*>[^<]*", "<table>").
      replaceAll("&#45;","-").
      replaceAll("& ", "&amp; ").
      replaceAll("–",    "-")// todo: make all possible decodings!!!

    val html6 = replaceAll(html5, DataWeDrop, "").replaceAll("(\\s*''''NL'''')+",  "''''NL''''")

    val html7 = html6.replaceAll("\\t*''''NL''''\\t*", "\n") // this is good for using in tests
    val result = html6.replaceAll("\\t*''''NL''''\\t*", " ").replaceAll("\\s+", " ")
    result
  }

  def extractProps(html: String): ParsingResult = {
    val vetted = cleanupHtml(html)
    try {
      val xml:Document = ConstructingParser.fromSource(Source.fromString(vetted), preserveWS = false).document()
      val almostReady = extractProps(xml)
      val EndsWithColon = "(.*):$".r
      val propsNoColon = almostReady.transformKeys {
        case EndsWithColon(k0) ⇒ k0.trim
        case k1                ⇒ k1.trim
      }
      val dirtyProps = propsNoColon.transformKeys(transformer)
      val cleanProps = dirtyProps.transformKeys(Linguist.normalize)

      val result = SuccessfullyParsed(cleanProps, html)
      result
    } catch {
      case oome: OutOfMemoryError ⇒
        FailedToParse("Out of memory in HtmlParser", html)
      case e: Exception ⇒
        e.printStackTrace()
        FailedToParse("Got exception in HtmlParser " + e, html)

    }
  }

  def extractProps(html: NodeSeq): Props = {
    val index = anchors(html)
    val propBuffer: PropBuffer = extractPropsWithPrefixes(Nil, index)(html)()
    propBuffer.processRemainingPrefixes.allProperties
  }

  def groupSegments(segments: List[PropBuffer]): List[List[PropBuffer]] = {
    def relatedSegments(x:PropBuffer, y: PropBuffer) = {
      val commonKeys = x.allProperties.commonKeys(y.allProperties)
      x == y || x.label == y.label ||
        commonKeys.nonEmpty
    }
    val groupIt:List[PropBuffer] ⇒ Stream[List[PropBuffer]] = groupByRelationship(relatedSegments)
    groupIt(segments).toList
  }

  def prependWithIndexesIfNecessary(segments: List[PropBuffer]): List[PropBuffer] = {
    val keySets = segments map (_.allProperties.fullKeys)
    val allKeys = (Set.empty[String]/:keySets)(_ ++ _)
    val allKeysAreDistinct = allKeys.size == keySets .map (_.size) .sum
    def enumerate(segments:List[PropBuffer]): List[PropBuffer] = segments.zipWithIndex map { case (s, i) ⇒ s addNumber (i+1)}
    val newSegments = if (allKeysAreDistinct || segments.size < 2) segments
    else enumerate(segments)
    newSegments
  }

  private[parsing] def extractPropsWithPrefixes(prefixes: List[String] = Nil, index: String Map String = Map.empty)(html: NodeSeq) = new {
    import HtmlContentExtractor.BIG_DEBUG
    // the following two values are good only when we run it through debugger; otherwise it is too expensive to calculate them
    val debugHtml  = BIG_DEBUG ?  html.toString | ""
    val debugIndex = BIG_DEBUG ? index.toString | ""

    val extractor = (node:Node) ⇒ extractNodeProps(index, prefixes)(node)
    val extracted = html map extractor
    val segments: List[PropBuffer] = extracted filter (!_.isEmpty) toList
    val segmentGroups: List[List[PropBuffer]] = groupSegments(segments)
    val indexedSegments = segmentGroups flatMap prependWithIndexesIfNecessary

    def fold(segments:List[PropBuffer]) = (justPrefixes("", prefixes) /: segments)(_ merge _)
    val result = fold(indexedSegments)
    def apply(): PropBuffer = result
  }

  private def nothing(label:String) = justPrefixes(label, Nil)
  private def EmptyTable = nothing("table")

  def styleOf(node: Node) = {
    s"style='${node.attribute("style") mkString " "}' class='${node.attribute("class") mkString " "}'"
  }

  def textOfElement(node: Node) = {
    val style = styleOf(node)
    val text = textOf(node)
    if (text.trim.endsWith(":") || !style.contains("bold")) text else s"$text:"
  }

  import Html._
  def plainText(nodes:Node*): String = cleanupText(
    nodes collect {
      case Text(value) ⇒ value
      case <a>{x@_*}</a> ⇒ plainText(x:_*)

      case span@ <span>{x@_*}</span> ⇒ textOfElement(span)
    } mkString " " trim
  )

  def extractPropsFromDL(nodes: Seq[Node]): Props = {
    val pairs = nodes.filter(Set("dd", "dt") contains _.label).toList.sliding(2,2).toList
    val kvs = pairs.collect {
      case (<dt>{key@_*}</dt>)::(<dd>{value@_}</dd>)::Nil ⇒ plainText(key:_*) → plainText(value:_*)
    }
    val complicatedData = pairs.collect {
      case (<dt>{key@_*}</dt>)::(<dd>{value@_*}</dd>)::Nil ⇒ (extractProps(key), extractProps(value))
    }
    val complicatedDataFound = complicatedData.forall(p ⇒ !p._1.isEmpty || !p._2.isEmpty)
    if (complicatedDataFound) {
      val result = Props.accumulate(complicatedData map (p ⇒ p._1 ++p._2))
      result
    } else {
      val goodKeysOnly = kvs.filter(!_._2.isEmpty)
      val result = props(goodKeysOnly.toMap)
      if (result.keysWithEmptyValues.nonEmpty) throw new IllegalArgumentException(s"Bad props from dl $nodes: $result")
      result
    }
  }

  trait ContainerParser {

    def node:Node
    def index: String Map String
    def id = attribute("id") of node
    def externalPrefix = id flatMap index.get

    def extractProperties(prefixes:List[String]) = new {
      val extracted = extractPropsWithPrefixes(prefixes, index)(node.child)()
      val converted = extracted.convertToProperties.withLabel(node.label)
      val cleanedUp = if (converted.properties.isEmpty) converted else converted.dropPrefixes

      // *a*      val result = converted//if (converted.properties.isEmpty) converted else converted.dropPrefixes
      val result = externalPrefix.fold (cleanedUp) (cleanedUp.prependPrefix)
      def apply() = result
    }
  }

  val ignoreThem = "aside,br,button,caption,cufon,fieldset,footer,noscript,script,select,style".split(",").toSet
  val useContentAndIndexIt = "ul"
  val useContent = "body,div,h1,h2,h3,h4,h5,head,html,iframe,li,p,section,td".split(",").toSet
  val ignorePrefixes = ""
  val plainText = "th,#entity,#pcdata,b,label,span,strong,textarea".split(",").toSet
  val regularClasses = "a,dl,img,meta,table".split(",").toSet

  private[parsing] def extractNodeProps(index:String Map String, prefixes: List[String] = Nil)(html: Node): PropBuffer = {
    val kind = html.label.toLowerCase

    val nodeHolder:NodeClass =
           if (useContentAndIndexIt contains kind) ArrayContainer(html, index)
      else if (useContent contains kind) Container(html, index)
      else if (ignorePrefixes contains kind) NonPrefixContainer(html)
      else if (plainText contains kind) PlainText(html)
      else if (regularClasses contains kind) Regular(html, index)
      else    Ignored(html)
    val result = nodeHolder parse prefixes
    result
  }

  case class TD(private val _source:Node)
    extends Cell(_source) { self ⇒
    override def withIndex(i: Int) = {
      val indexPrefix = numberKey(i)

      new TD(_source) {
        override lazy val data: PropBuffer =
          new PropBuffer(self.data.label,
            self.data.properties.addPrefix(indexPrefix),
            Nil
          )
      }
    }
  }

  case class TH(private val _source:Node) extends Cell(_source) { self ⇒
    override def withIndex(i:Int) = new TH(_source) {
      override lazy val data:PropBuffer =
        new PropBuffer(self.data.label,
          self.data.properties.addNumber(i),
          Nil)
    }
    override def plainText = cleanupText(data.rawPrefixes.mkString(" "))
    override def text = cleanupText(data.prefixes.mkString(" ").replaceAll(":$", "")) // colon in the end o, get rid of it
  }

  private def cellOf(cell: Node) = cell.label match {
    case "td" ⇒ TD(cell)
    case "th" ⇒ TH(cell)
    case _    ⇒ throw new IllegalArgumentException(s"unexpected cell node $cell")
  }

  private def rowsOf(table:NodeSeq): Seq[Node] = table.theSeq collect { case t @ <tr>{r@ _*}</tr> ⇒ t }

  def cellsOf(htmlRows: NodeSeq): List[Row] = {
    val rawRows: Seq[Node] = rowsOf(htmlRows)
    val res = rawRows map (tr ⇒
      Row(
        tr.child collect {
          case node@(<td>{x@_*}</td>) ⇒ cellOf(node)
          case node@(<th>{x@_*}</th>) ⇒ cellOf(node)
        } toList, tr)
      ) toList

    if (res.length > 1) matchHeaderDataRows(res) else res

  }

  case class ValueMatrix(valueMatrix: ArrayBuffer[ArrayBuffer[Cell]]) {
    val matrix = valueMatrix

    def doesFirstColumnExist(): Boolean = {
      matrix.nonEmpty && matrix(0).nonEmpty
    }

    def isColumnEmpty(columnNumber: Int): Boolean = {
      val isColEmpty = matrix.forall((e: ArrayBuffer[Cell]) ⇒ e(columnNumber).saysItsEmpty)
      isColEmpty
    }

    def deleteColumn(columnNumber: Int): Unit = {
      matrix.foreach(_.remove(columnNumber))
    }

    def addToValueMatrix(rows: List[Row]): Unit = {
      for (count ← rows.indices) {
        val currentRow = rows(count)
        val cellsInCurrentRow = currentRow.cells
        val rowBuffer = matrix(count)
        for (cellCount ← cellsInCurrentRow.indices) {
          val currentCell = cellsInCurrentRow(cellCount)
          val attributeMap = currentCell.source.attributes.asAttrMap
          if (!currentCell.isEmpty || currentCell.hasData) {
            rowBuffer(cellCount) = currentCell
          } else if (attributeMap.nonEmpty) {
            val newSource = new Elem(null, "td", currentCell.source.attributes, scala.xml.TopScope, false, Text("hasAttribute"))
            rowBuffer(cellCount) = TD(newSource)
          }
        }
        if (cellsInCurrentRow.length < rowBuffer.length) {
          for (leftOverCount ← cellsInCurrentRow.length until rowBuffer.length) {
            rowBuffer(leftOverCount) = TD(<td>Empty</td>)
          }
        }
      }
    }

    def cleanMatrix(): Unit = {
      // for first or last column : if header is missing and all data are empty delete the column
      checkFirstLastEmptyColumn()
    }

    /**
      * for first or last column : if header is missing and all data are empty delete the column
      */
    def checkFirstLastEmptyColumn(): Unit = {
      if (doesFirstColumnExist && isColumnEmpty(0)) deleteColumn(0)
      if (doesFirstColumnExist && isColumnEmpty(matrix(0).length - 1)) deleteColumn(matrix(0).length - 1)
      if (doesFirstColumnExist && (isColumnEmpty(0) || isColumnEmpty(matrix(0).length - 1))) {
        checkFirstLastEmptyColumn()
      }
    }

    /**
      * Regenerate list of Rows which are properly matched and cleaned so that Props
      * can be generated without glitches.
      * @param rows List[Row]
      * @return List[Row]
      */
    def createRowsFromMatrix(rows: List[Row]): List[Row] = {
      val rowsList = new ListBuffer[Row]
      for (rowCount ← matrix.indices) {
        val row: Row = rows(rowCount)
        // TODO(Rupa/Vlad): no nulls! give name to false
        var nodeVal = Elem(null, "tr", row.source.attributes, scala.xml.TopScope, false)
        val rowBuffer = matrix(rowCount)
        val cellList = new ListBuffer[Cell]
        for (cellsCount ← rowBuffer.indices) {
          var cell = rowBuffer(cellsCount)
          // TODO(Rupa/Vlad): take care of this "Empty" text" and "hasAttribute" text
          if (!cell.text.equals("Empty")) {
            if (cell.text.equalsIgnoreCase("hasAttribute")) {
              cell = TD(Elem(null, "td", row(cellsCount).source.attributes, scala.xml.TopScope, false))
            }
            cellList += cell
            nodeVal = nodeVal.copy(child = nodeVal.child ++ cell.source)
          }
        }
        rowsList += new Row(cellList.toList, nodeVal)
      }
      rowsList.toList
    }
  }

  /**
    * Create a matrix from a given List of rows.
    * Clean the list of rows to remove empty rows abd columns. The columns which are
    * missing data are filled iwth place holder strings so that existing methods can
    * properly create Props and not jumble up the uneven rows with missing data.
    * Finally another List of Rows is created which is passed on to the existing
    * function In the HTMLContentExtractor to create Props.
    * @param rowsIn List[Row]
    * @return  List[Row]
    */
  def matchHeaderDataRows(rowsIn: List[Row]): List[Row] = {
    val rows = cleanEmptyRows(rowsIn)
    if (rows.nonEmpty) {
      val headerRowLength = rows(0).size
      val returnRows = if (minRowLength(rowsIn) >= headerRowLength) {
        val valueMatrix = new ValueMatrix(ArrayBuffer.fill[Cell](rows.length, maxRowLength(rows))(TD(<td></td>)))
        valueMatrix.addToValueMatrix(rows)
        valueMatrix.cleanMatrix()
        valueMatrix.createRowsFromMatrix(rows)
      } else {
        Logging.debug("Data rows have less values than header, impossible to match correctly, leave rows list as is")
        rows
      }
      returnRows
    } else {
      rows
    }
  }

  /**
    * Remove empty rows from a List of Rows
    * @param rowsIn
    * @return List[Rows]
    */
  def cleanEmptyRows(rowsIn: List[Row]): List[Row] = {
    val rows = rowsIn filterNot (row ⇒ isRowEmpty(row.source))
    rows
  }

  /**
    * Returns the length of longest Row from a List of Rows
    * @param rows
    * @return length:Int
    */
  def maxRowLength(rows: List[Row]): Int = {
    rows map (_.size) max
  }

  /**
    * Returns the length of shortest Row from a List of Rows
    * @param rows
    * @return length:Int
    */
  def minRowLength(rows: List[Row]): Int = {
    rows map (_.size) min
  }

  def extractPropsFromVerticalTable(rows: Seq[Row]): PropBuffer = {
    val innerPropsInCells = rows.flatMap(row ⇒ row.cells.map(_.data.properties))
    val p2 = rows.map(_.buildProps)
    val innerPropsWithPrefixes = Props.accumulate(p2)
    val innerProps = Props.accumulate(innerPropsInCells)
    val rowsWithProps = rows map (row ⇒ row.trimToSize(row.size/2*2)) filter (_.size > 1)
    val matrix = rowsWithProps map (_.cells)
    val cleanedUpRows =
      try {
        val transposedMatrix = matrix .transpose .zipWithIndex
        val empties = transposedMatrix .filter(_._1.forall(_.isEmpty)) .map(_._2)
        rowsWithProps map (_.deleteCellsAt(empties))
      } catch {
        case e:Exception ⇒ rowsWithProps
      }
    val pairs:Seq[(String, String)] = cleanedUpRows flatMap (_.kvPairs)
    val map = pairs.toMap filter (!_._2.isEmpty)
    new PropBuffer("table", innerProps ++ props(map) ++ innerPropsWithPrefixes, Nil)
  }

  def isThStyle(node: Node) = {
    val text = node.text.trim
    val hasPropertyName = (text endsWith ":") && text.count(':'==) == 1
    val isStrong = node.toString contains "<strong"
    val isTitleClass = node.attribute("class").toString.toLowerCase contains "title"
    hasPropertyName || isStrong || isTitleClass
  }

  def cellKind(cell:Node) = cellWidth(cell) match {
    case 1 ⇒
      val text = cell.text.trim
      if (isThStyle(cell)) "th" else if (text.isEmpty) "-" else cell.label
    case n if n%2 == 0 ⇒ ""
    case _ ⇒ "..."
  }

  private def rowIsGoodForVerticalFormat(r:Node) = {
    val collected = r.child collect {
      case cell@(<th>{x@_*}</th>) ⇒ cellKind(cell)
      case cell@(<td>{x@_*}</td>) ⇒ cellKind(cell)
    }
    val desc = collected.mkString("")
    val answer =
        desc.matches("(th(td|-)+)*") ||
        desc.matches("tdth-?") ||
        desc.matches("tht.-?") ||
        desc.matches("ththtdth") ||
        desc.matches("-*tdth") ||
        desc.matches("tdthtdth")
    answer
  }

  def stylesOf(column: Seq[Node]): Set[String] = {
    column .map (cell ⇒ ""+cell.attribute("style")+cell.attribute("class")) .toSet
  }

  def isRowEmpty(row: Node) = row.text.trim.isEmpty

  def weHaveTwoUniformColumns(rows: Seq[Node]) = {

    val matrix = rows filterNot isRowEmpty collect {
      case row ⇒ row.child.theSeq.toList
    }

    val isUniform = matrix.map(_.size).toSet.size == 1
    isUniform && {
      val columns = matrix.transpose.toList
      val styles = columns map stylesOf
      styles.size == 2 && styles.forall(_.size == 1) && {
        styles match {
          case s1::s2::Nil ⇒ s1 != s2
          case oops ⇒ false
        }
      }
    }
  }

  def isHeaders(htmlRows:NodeSeq) = {
    val rows = rowsOf(htmlRows)
    val itIs = rows match {
      case row::Nil ⇒
        row.child forall (_.label == "th")
      case _ ⇒ false
    }
    itIs
  }

  def isVertical(table:NodeSeq) = {
    val rows = rowsOf(table)
    val bad = rows find (!rowIsGoodForVerticalFormat(_))
    bad foreach { // for testing
      rowIsGoodForVerticalFormat
    }
    bad.isEmpty || weHaveTwoUniformColumns(table)
  }

  /**
    * Checking if the table has Massachusetts format.
    * It is Massachusetts if it consists of 3-cell rows: key, empty cell, value.
    * Easy, but unusual
    *
    * @param table list of rows, the table to check
    */
  def isMassachusettsStyle(table:List[Row]) = {
    val bad = table find (!_.isMassachusettsStyle)
    //    val foundbad = rows filterNot rowIsGoodForVerticalFormat
    bad.isEmpty
  }

  def isNorthDakotaStyle(table:List[Row]) = {
    val bad = table find (!_.isNorthDakotaStyle)
    //    val foundbad = rows filterNot rowIsGoodForVerticalFormat
    bad.isEmpty
  }

  def extractPropsFromMassachusetts(table:List[Row]): PropBuffer = {
    val propMap = table map (_.kvMassachusettsStyle) collect {
      case Good(kv) ⇒ kv
    } toMap

    new PropBuffer("table", props(propMap))
  }

  def extractPropsFromNorthDakota(table:List[Row]): PropBuffer = {
    val propsRows = table map (_.kvNorthDakotaStyle) collect {
      case Good(kvs) ⇒ kvs
    }

    val propMap = propsRows.flatten.toMap

    new PropBuffer("table", props(propMap))
  }

  def isLincolnStyle(table: List[Row]) = {
    val signatures = table map (_.signature)
    val pairSignatures = signatures.grouped(2)
    val yes = table.length %2 == 0 && table.length > 2 && pairSignatures.forall {
      // the case where header rows are followed by values rows
      case h::v::Nil ⇒
        val isOurCase = h._1 == "th" && h._2 == v._2 && v._1 == "td" && h._2 > 1
        isOurCase
      case otherwise ⇒ false
    }
    yes
  }

  def extractPropsFromLincoln(table: List[Row]): PropBuffer = {

    // a row of keys followed by a row of values, repeated
    val textMatrix:List[List[String]] = table.map (_.cells.map(_.text))

    val rowPairs:List[List[List[String]]] = textMatrix.grouped(2).toList

    val propRows = rowPairs collect {
      case (keys:List[String])::(values:List[String])::Nil ⇒
        val map = keys.zip(values).toMap
        props(map)
    }

    val allProps:Props = (Props.empty/:propRows)(_++_)

    new PropBuffer("table", allProps)
  }

  def isaBunchOfKeyValuePairs(rows: Seq[Row]): Boolean = {
    val allCells = rows flatMap (row ⇒ row.cells)
    val cellsWithData = allCells filter (!_.isEmpty)
    val total = cellsWithData.size
    val cellsWithKeys = cellsWithData filter (_.text endsWith ":")
    total > 0 && total == cellsWithKeys.size * 2
  }


  def extractKeyValuePairs(rows: Seq[Row]): PropBuffer = {
    val allCells: Seq[(Int,Int,Cell)] =
      rows.zipWithIndex flatMap { case (row, i) ⇒
        row.cells.zipWithIndex map { case (cell, j) ⇒ (i, j, cell)}
      }

    val cellsWithData: Seq[(Int,Int,Cell)] = allCells filter (!_._3.isEmpty)

    val (cellsWithKeys, cellsWithValues) = cellsWithData.partition(_._3.text endsWith ":")
    val valueMap = cellsWithValues .map(t ⇒ (t._1,t._2)→t._3.text) toMap
    val kvs = cellsWithKeys map {
      case (i,j,cell) ⇒
        valueMap.get((i,j+1)) orElse valueMap.get((i+1,j)) map (cell.text.dropRight(1) → _)
    } collect {
      case Some(kv) ⇒ kv
    }

    new PropBuffer("table", props(kvs toMap), Nil)
  }

  def extractPropsFromTableRows(rows: NodeSeq): PropBuffer = {
    detectAndExtractTable(rows)._2
  }

  def detectAndExtractTable(rows: NodeSeq): (String, PropBuffer) = {
    val allCells = cellsOf(rows)
    if (allCells.isEmpty) {
      ("Empty", EmptyTable)
    } else if (isHeaders(rows)) {
      ("HeaderTable", (new PropBuffer.HeaderTableBufferBuilder(allCells))())
    } else if (isVertical(rows)) {
      ("Vertical", extractPropsFromVerticalTable(allCells))
    } else if (isMassachusettsStyle(allCells)) {
      ("MA", extractPropsFromMassachusetts(allCells))
    } else if (isLincolnStyle(allCells)) {
      ("Lincoln", extractPropsFromLincoln(allCells))
    } else if (isNorthDakotaStyle(allCells)) {
      ("ND", extractPropsFromNorthDakota(allCells))
    } else ("Regular", extractPropsFromStandardTable(allCells))
  }

  def extractPropsFromStandardTable(cells: List[Row]) = (new PropBuffer.RegularTableBufferBuilder(cells))()

  type ColDesc = List[(String, Int)]

  protected[parsing] def extractPropsFromTableRows(allRows:Seq[Row]): PropBuffer = {
    val rows = allRows.filter(!_.text.trim.isEmpty) // ignore spacer rows
    val headerHeight = rows.takeWhile(r ⇒ r.size == 1 ||r.cells.tail.exists((_:Cell).width > 1)).size + 1
    val stylesPerRow = rows map (r ⇒ r.cells.map(_.style))
    val varietyOfStyles = stylesPerRow.toSet
    if (varietyOfStyles.toSet.size == 1 && rows.head.isaKeyFollowedByValues) {
      val propsOfRows = rows map (_.keyFollowedByValues)
      val result = new PropBuffer("table", Props.accumulate(propsOfRows))
      return result
    }
    val (headerRows, dataRowsCandidates) = rows.splitAt(headerHeight)
    val dataRows: List[Row] = dataRowsCandidates takeWhile (_.size > 1) toList

    if (headerRows.isEmpty) return EmptyTable
    if (dataRows.isEmpty) return EmptyTable
    val haveEmptyHeaderCell = headerRows.exists(_(0).isEmpty)
    val haveHorizontalTable = !haveEmptyHeaderCell && {
      val cellsOfFirstDataRow = dataRows.head.cells
      val styles = cellsOfFirstDataRow.map(cell ⇒ cell.style
        /*TODO(vlad): ignore class; some tables have a variety of classes but still are horizontal in nature + " " + html.attribute("class")*/)
      val havePropsInside = cellsOfFirstDataRow.exists(_.text.matches(PropertyFormat))
      !havePropsInside /*&& styles.tail.forall(styles.head ==)*/
    }

    val titleWidth = if (haveHorizontalTable) 0 else headerRows find(_.size > 1) map(_(0).width) getOrElse 1
    val headersInRows:Seq[ColDesc] = headerRows.map(row ⇒ row.cells.flatMap(cell ⇒ List.fill(cell.width)((cell.text, cell.height))))
    def mergeRows(row1: ColDesc, row2:ColDesc):ColDesc = { val res =
      if (row2.isEmpty) row1 else row1 match { // on empty: try to return Nil? ***
        case Nil    ⇒ row2
        case h1::t1 ⇒
          if(h1._2 == 1) row2.head :: mergeRows(t1, row2.tail) else (h1._1, h1._2-1) :: mergeRows(t1, row2)

      }
      res
    }

    val normalizedHeaders = ((headersInRows.head::Nil) /: headersInRows.tail)( (x, y) ⇒ mergeRows(x.head,y)::x) .reverse
    if (normalizedHeaders.isEmpty) return EmptyTable
    val nh0 = normalizedHeaders.head.size
    if (normalizedHeaders.exists(_.size != nh0)) return EmptyTable
    def convertHeaderCol(col: ColDesc): String = col.map(_._1).map(trimKey).mkString(".")
    val headers:List[String] = normalizedHeaders.transpose.map(convertHeaderCol).drop(titleWidth)
    val dataWidthReportedByHeaders = headers.size
    val names = extractRowNames(dataRows, titleWidth)

    val explicitNamePrefixes = names.reverse map(row ⇒ row.map(_._1.replaceFirst(":$", "")).mkString("."))
    val dataWidthReportedByDataRows = dataRows.map(_.width).max - titleWidth

    val dataWidth = {
      val colWidth = math.min(dataWidthReportedByDataRows, dataWidthReportedByHeaders)
      val dataRowWidth = maxRowLength(dataRows)
      math.min(colWidth,dataRowWidth)
    } // this is strange, what if we have outstanding data?
    val rowsGrouped:List[List[Row]] = groupPrefix(dataRows)(_.size >= dataWidth)
    val rowsWithExtraData: List[(Row, Props)] = rowsGrouped collect {
      case (row:Row)::(tail:List[Row]) ⇒ (row, {
        val rowsWithProps = tail
        val propsList = rowsWithProps map (_.buildProps)
        val props = Props.accumulate(propsList)
        props
      })
    }
    val rowsWithMainContent = rowsWithExtraData map (_._1)
    val namePrefixes = buildNamePrefixesForTableData(rowsWithMainContent, dataWidth, explicitNamePrefixes)
    val result = buildPropertiesFromTableRows(rowsWithExtraData, namePrefixes, dataWidth, headers)
    result
  }

  def extractRowNames(dataRows: Seq[Row], titleWidth: Int):List[ColDesc] = {

    def nameRecord(row: Row, width: Int): ColDesc = {
      val seq = row.cells take width
      (seq map ((cell: Cell) ⇒ (cell.text, cell.height))).toList
    }

    def updateNameRec(rec1: ColDesc, rec2: ColDesc): ColDesc = {
      if (rec2.isEmpty) return Nil
      val init: ColDesc = Nil
      val z: (ColDesc, ColDesc) = (init, rec2)
      def update(outAndNextRow: (ColDesc, ColDesc), current: (String, Int)): (ColDesc, ColDesc) = {
        val out: ColDesc = outAndNextRow._1
        val nextRow: ColDesc = outAndNextRow._2
        require(nextRow.nonEmpty, s"Wrong column description, can't have empty row: $outAndNextRow / $current")
        val left = current._2 - 1
        val next: (ColDesc, ColDesc) = if (left > 0) (out :+(current._1, left), nextRow)
        else if (nextRow.isEmpty) throw new IllegalArgumentException(s"Failed to merge $rec1 and $rec2")
        else (out :+ nextRow.head, nextRow.tail)
        next
      }

      val updated: (ColDesc, ColDesc) = (z /: rec1)(update) // you won't believe how many times I had to declare intermediate vals
      updated._1
    }
    val first = nameRecord(dataRows.head, titleWidth)
    val names = ((first :: Nil) /: dataRows.tail)((x: List[ColDesc], y: Row) ⇒ {
      updateNameRec(x.head, nameRecord(y, titleWidth)) :: x
    }).toList
    names
  }

  def buildNamePrefixesForTableData(rowsWithContent: Seq[Row], dataWidth: Int, explicitNamePrefixes: Seq[String]): Seq[String] = {
    val column1 = rowsWithContent map (row ⇒ row(row.size - dataWidth).text)

    val namePrefixes = if (column1.forall(_ contains ":")) {
      explicitNamePrefixes zip column1 map {
        case (name, cell) ⇒ name + "." + trimKey(cell.split(":").head)
      }
    } else explicitNamePrefixes
    namePrefixes
  }
  /*
      if (titleWidth == 0 && dataRows.length > 1) { // TODO: this is suspicious, separate treatment for the case when data have just one row...
        def asDesc(i:Int): ColDesc = (""+i, 1)::Nil
        val rowNumbers: Seq[ColDesc] = (1 to dataRows.length) map asDesc
        return rowNumbers.toList
      }

   */

  def buildPropertiesFromTableRows(rowsWithContent: Seq[(Row, Props)], namePrefixes: Seq[String], dataWidth: Int, headers: Seq[String]): PropBuffer = {
    val needEnumerateRows = rowsWithContent.length > 1 && (namePrefixes forall (_.isEmpty)) // TODO(vlad): this is suspicious actually; what's the difference, one row or two rows
    val rowKeyPrefix = if (needEnumerateRows) (rowNumber: Int) ⇒ Props.numberKey(rowNumber) + "." else (rowNumber: Int) ⇒ ""

    def buildKey(rowNumber: Int, header: String, rowName: String) = {
      rowKeyPrefix(rowNumber) + header + "." + trimKey(rowName)
    }

    val allPropsListed = for {j ← rowsWithContent.indices
                              (row, extraProps:Props) = rowsWithContent(j)
                              name = namePrefixes(j)} yield {
      val kvs = for {
        i ← 0 until dataWidth
        header = headers(i)
      } yield {
        buildKey(j + 1, header, name) → row(row.size - dataWidth + i).text.split(":").last.trim
      }
      val keyValuePairs = kvs filter (!_._2.isEmpty)
      val rowProps = props(keyValuePairs.toMap)
      rowProps ++ extraProps.addPrefix(rowKeyPrefix(j+1))
    }
    val allProps = Props.accumulate(allPropsListed)
    new PropBuffer("table", allProps)
  }

  def transformerFromMap(map: String Map String)(key: String) = map getOrElse(key, key)

  def withTransformer(map: String Map String):HtmlContentExtractor = withTransformer(transformerFromMap(map) _)

  def withTransformer(keyTransformer: String ⇒ String):HtmlContentExtractor = new HtmlContentExtractor {
    override def transformer = keyTransformer
  }
}



object HtmlContentExtractor extends HtmlContentExtractor {

  var BIG_DEBUG = true

  sealed trait ParsingResult {
    def map(f:Props ⇒ Props):ParsingResult = this
    def toResult: Result[Props]
    def toResultKeepingSource: Result[(Props, String)]
  }

  final case class SuccessfullyParsed(props:Props, source:String) extends ParsingResult {
    override def map(f:Props ⇒ Props) = SuccessfullyParsed(f(props), s"(mapped via $f from) " + source)
    override def toResult = Good(props)
    override def toResultKeepingSource = Good(props, source)
  }

  private[HtmlContentExtractor] sealed trait ParsingFailure extends ParsingResult {
    def oops:Bad[Nothing]
    override def toResult = oops
    override def toResultKeepingSource = oops
  }
  // todo: have a class that captures exception (in Bad)
  final case class FailedToParse(explanation:Any, badHtml: String) extends ParsingFailure {
    override def oops:Bad[Nothing] = Result.error(explanation + "\n" + badHtml)
  }

  final case class NothingToParse(message:Any) extends ParsingFailure {
    override def oops:Bad[Nothing] = Result.error(message)
  }

  def traverse(results: Seq[ParsingResult]): Result[Seq[Props]] = {
    val goodOnes = results.collect {case SuccessfullyParsed(props, _) ⇒ props}
    val failures = results.collect {case FailedToParse(x, _) ⇒ x; case NothingToParse(x) ⇒ x}
    if (failures.isEmpty) Good(goodOnes) else Result.error(failures)
  }
}

private[parsing] abstract class NodeClass(html:Node) {
  def parse(prefixes:List[String] = Nil): PropBuffer
}

private[parsing] final case class Ignored(node:Node) extends NodeClass(node) {
  override def parse(prefixes:List[String]) = justPrefixes("", prefixes)
}

private[parsing] final case class ArrayContainer(node:Node, index: String Map String)
  extends NodeClass(node) with ContainerParser {

  def isaLi(node:Node) = node.label == "li"

  override def parse(prefixes:List[String]): PropBuffer = {
    val out:PropBuffer = node match {
      case <ul>{ x@ _*}</ul> ⇒
        val listElements:Seq[Node] = node.child filter isaLi

        val contentList = listElements map extractNodeProps(index)

        if (contentList.forall(!_.hasData)) {
          contentList match {
            case Nil      ⇒ justPrefixes("ul", prefixes)
            case one::Nil ⇒ extractProperties(prefixes)()
            case keyNode::valueNode::Nil if !keyNode.prefixes.mkString.endsWith(".") ⇒
              val key = keyNode.prefixes mkString " "
              val value = valueNode.prefixes mkString " "
              val pa = new PropBuffer("ul", props(key → value), prefixes)
              pa

            case moreThanOne ⇒
              val newMap: Map[String, String] = contentList.zipWithIndex collect {
                case (row, i) ⇒ Props.numberKey(i + 1) → (row.prefixes mkString " ")
              } toMap
              val goodNewMap = newMap filter (!_._2.isEmpty)
              val pb = new PropBuffer("ul", props(goodNewMap), prefixes)
              pb
          }

        } else {
          val segments:Seq[PropBuffer] = contentList.zipWithIndex map {
            case (row, i) ⇒ row prependPrefix Props.numberKey(i+1)
          }

          val result = (justPrefixes("", prefixes) /: segments)(_ merge _)
          result
        }
    }
    out
  }
}


private[parsing] final case class Container(node:Node, index: String Map String)
  extends NodeClass(node)
    with ContainerParser {
  override def parse(prefixes:List[String]) = extractProperties(prefixes)()
}

private[parsing] final case class NonPrefixContainer(node:Node) extends NodeClass(node) {
  override def parse(prefixes:List[String]) = {
    val extracted = extractPropsWithPrefixes(prefixes)(node.child)()
    if (extracted.properties.isEmpty) justPrefixes(node.label) else extracted
  }
}

private[parsing] final case class PlainText(node:Node) extends NodeClass(node) {

  override def parse(prefixes:List[String]) = {
    val plainText = textOfElement(node)
    val result = justPrefixes(node.label, (plainText :: prefixes) filter (!_.isEmpty) map transformer)
    result
  }
}

private[parsing] final case class Regular(node:Node, index:String Map String) extends NodeClass(node) {
  import ExtractorOps._

  def parseMeta(prefixes:List[String]) = {
    val attrs = Props(node.attributes.asAttrMap.map{case (k,v) ⇒ k.toLowerCase → v})
    def attr(name:String):Result[String] = (attrs @@ name) filter (!_.isEmpty)
    val newBuffer =
      for (kvPair ← attr("name") andAlso attr("content"))
        yield new PropBuffer("meta", Props(Map(kvPair)), prefixes)

    newBuffer getOrElse justPrefixes("", prefixes)
  }

  override def parse(prefixes:List[String]): PropBuffer = {
    val out:PropBuffer = node match {
      case <dl>{     x@ _*}</dl>      ⇒
        val propsFromDL = extractPropsFromDL(x)
        if (propsFromDL.keysWithEmptyValues.nonEmpty) throw new IllegalArgumentException(s"Bad props from dl $x")
        new PropBuffer(node.label, propsFromDL, prefixes)

      case <table><caption>{stuff}</caption><tbody>{rows @ _*}</tbody></table> ⇒
        extractPropsFromTableRows(rows) ++ prefixes

      case <table><tbody>{rows @ _*}</tbody></table> ⇒
        extractPropsFromTableRows(rows) ++ prefixes

      case <tbody>{       rows @ _*        }</tbody> ⇒
        extractPropsFromTableRows(rows) ++ prefixes

      case <table>{       rows @ _*        }</table> ⇒
        val extracted = extractPropsFromTableRows(rows)
        extracted ++ prefixes

      case <img>{x@ _*}</img> ⇒
        val alt = attribute("alt") of node getOrElse ""
        if (alt.isEmpty) justPrefixes("", prefixes) else new PropBuffer("img", Props.empty, alt :: prefixes)

      case <META>{x@ _*}</META> ⇒ parseMeta(prefixes)
      case <meta>{x@ _*}</meta> ⇒ parseMeta(prefixes)

      case <a>{   x@ _*}</a>  ⇒
        val newPrefix = trimKey(x.text)
        val href = (node \ "@href") mkString " "
        if (href.startsWith("#") && href.length > 1 && index.contains(href.tail) && !newPrefix.isEmpty)
          new PropBuffer("a", props(newPrefix→href), prefixes)
        else {
          val newPrefixes = newPrefix.isEmpty ? prefixes | newPrefix :: prefixes
          justPrefixes(node.label, newPrefixes)
        }

      case atom:scala.xml.Atom[_]  ⇒ new PropBuffer(node.label, Props.empty, atom.text :: prefixes)

    }
    out
  }
}

abstract class Cell(val source: Node) {
  import ExtractorOps._

  def style =  source.label + " " + source.attribute("style").map(_.toString.replaceAll("display:\\s*\\w+", "")).filter(!_.isEmpty)

  def width  :Int    = cellWidth(source)
  def height :Int    = cellHeight(source)
  lazy val data: PropBuffer = extractNodeProps(Map.empty)(source)
  def text   :String = cleanupText(data.prefixes.headOption.getOrElse(""))
  def plainText      = cleanupText(data.rawPrefixes.headOption getOrElse "")
  def fullText      = textOf(source)
  def hasData = data.hasData
  def isEmpty = text.trim.isEmpty //&& data.isEmpty
  def saysItsEmpty = (isEmpty && !hasData) || text.containsIgnoreCase("Empty")
  def withIndex(i:Int): Cell
}

case class Row(cells: List[Cell], source: Node) {

  def trimToSize(n: Int) = Row(cells take n, source)

  def deleteCellsAt(empties: Seq[Int]): Row = {
    val goodCells = cells.zipWithIndex filter { case (cell, i) ⇒ !empties.contains(i)}
    Row(goodCells map (_._1), source)
  }

  def kvMassachusettsStyle: Result[(String, String)] = textSegments match {
    case key::""::value::Nil if (key.nonEmpty && value.nonEmpty) ⇒ Good(key → value)
    case _ ⇒ Empty
  }

  def isMassachusettsStyle = kvMassachusettsStyle.isGood

  def kvNorthDakotaStyle: Result[Seq[(String, String)]] = {

    def kvp(seq:List[String]): Result[List[(String, String)]] = seq match {
      case k::v::tail if k.endsWith(":") ⇒
        kvp(tail) map (t ⇒ (k.dropRight(1)→v)::t)
      case Nil ⇒
        Good(Nil)
      case somethingElse ⇒
        val bs = somethingElse
        Result.error(s"Not from North Dakota: $somethingElse")
    }

    val candidate = kvp(textSegments filter (_.trim.nonEmpty)) filter (_.nonEmpty)
    candidate
  }

  def isNorthDakotaStyle = kvNorthDakotaStyle.isGood

  def textSegments = cells.map(_.text)
  def text = textSegments mkString " "
  def width  = (0/:cells)(_+_.width)
  def height = (0/:cells)(_+_.height)
  def size = cells.size
  def apply(i:Int) = cells(i)
  def hasData = cells.exists(_.hasData)
  def contentKind = {
    val collected = cells collect {
      case TH(src) ⇒ "th"
      case TD(src) ⇒ "td"
    }
    collected.toSet
  }
  def justKeys   = contentKind == Set("th")
  def justValues = contentKind == Set("td")

  lazy val cellPairs: Iterator[List[Cell]] = cells sliding(2, 2)

  def buildProps: Props = {
    val cx = (null::cells)tail // TODO(vlad): this is a very stupid hack that works... investigate!

    val cp = cx sliding(2, 2) toList

    val prefixedProperties:List[(String, Props)] = cp collect { case head :: tail :: Nil ⇒ (head.text, tail.data.allProperties)} filter {_._2.nonEmpty} //toList

    val acceptablePrefixedProperties = prefixedProperties .map {
      case (k, p) ⇒ (k.split(":")(0), p)
    }filter (_._1.nonEmpty)

    val properties = acceptablePrefixedProperties map { case(prefix, props) ⇒ props.addPrefix(prefix)}

    Props.accumulate(properties)
  }

  private lazy val allKvPairs:Seq[(String, String)] = cellPairs collect{case head::tail::Nil ⇒
    val ht = (head.text, tail.plainText)
    ht
  } toList
  lazy val kvPairs = allKvPairs map { case(k, v) ⇒ (k.split(":")(0), v)} filter(_._1.nonEmpty)
  def isaKeyFollowedByValues = {
    val style = cells map (_.style)
    val firstColumnStyle = style.head
    style.tail.toSet.size == 1 && firstColumnStyle != style.tail.head
  }
  def keyFollowedByValues: Props = {
    val key = cells.head.text
    val values = cells.tail map (_.text)
    Props.fromList(values).addPrefix(key)
  }

  private def isAllHeaders: Boolean = {
    isThStyle(source) || (cells.map(cell ⇒ cellKind(cell.source)) forall ("th"==))
  }

  private def isAllValues: Boolean = {
    cells.map(cell ⇒ cellKind(cell.source)) forall (kind ⇒ kind == "td" || kind == "" || kind == "-")
  }

  lazy val signature = {
    val discovered = (if (isAllHeaders) "th" else if (isAllValues) "td" else "?", cells.takeWhile(!_.isEmpty).length)
    discovered
  }

}
