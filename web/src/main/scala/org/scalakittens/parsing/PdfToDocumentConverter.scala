/**
 * We already have HtmlContentParser that extracts contents from tables and key-value kinds of data;
 * we also have SimpleTextDocument that extracts data, less automatically, from html produced by
 * pdf2html tool.
 * The problem with the first is that it won't deal with HTML5 kind of format;
 * the problem with the second is that the result of pdf->html transformation is pretty mediocre, and
 * it loses a lot of information on how data are separated.
 *
 * So I decided to write a new one, which takes the result of rendering pdf via pdf.js (the thing
 * that is used in mozilla by default), and converts into SimpleTextDocument, but in a more
 * sophisticated way.
 *
 * pdf.js produces a sequence of divs, each one has coordinates, scale, and rotation angle - but these divs
 * are not sorted by x or y, they are random. I'll try to produce a list of lines, each consisting of a list
 * of segments. That is, SimpleTextDocument.
 *
 * Then maybe I'll merge the code.
 */
object PdfToDocumentConverter {
  val VerticalPrecision = 2.0 // two rows whose y coords differ less than VerticalPrecision are merged into one
  val HorizontalPrecision = 1.0 // two columns whose x coords differ less than HorizontalPrecision are considered one

//  def fromPdfHtml(html: String): Result[SimpleTextDocument] = {
//    val pages = html.split("<p style=\"page-break-after:always;\"></p>")
//    val docOpt = fromPdfHtml(pages.toList)
//    docOpt
//  }
  def fromPdfHtml(html: String): Result[SimpleTextDocument] = {
    val pages: List[String] = splitPages(html)
    val docOpt = PdfToDocumentConverter.fromPdfHtml(pages)
    docOpt
  }

  def splitPages(html: String): List[String] = {
    val pages = html match {
      case inp if (shouldUsePageBreakAfterSplit(inp)) ⇒
        inp.split("<p style=\"page-break-after:always;\"></p>").toList
      case inp if (shouldUsePageContainerSplit(inp)) ⇒
        extractPageDivsFromHtml(inp)
      case _ ⇒
        List(html)
    }
    pages
  }

  def extractPageDivsFromHtml(html: String): List[String] = {
    val source: ConstructingParser = ConstructingParser.fromSource(Source.fromString(HtmlContentExtractor.cleanupHtml(html)), preserveWS = false)
    val xml: Document = source.document()
    val divs: NodeSeq = xml \\ "div"
    val pages = divs filter {
      _ \ "@id" exists (_.text startsWith "pageContainer")
    }
    pages match{
      case pgList if (!pgList.isEmpty) ⇒
        pgList.map(_.toString).toList
      case _ ⇒
        List(html)
    }
  }
  def shouldUsePageBreakAfterSplit(html: String): Boolean = {
    html.contains("page-break-after:always;")
  }
  def shouldUsePageContainerSplit(html: String): Boolean = {
    html.contains("\"pageContainer")
  }

  def fromPdfHtml(html: List[String]): Result[SimpleTextDocument] = {
    val cleanedUp = html map HtmlContentExtractor.cleanupHtml
    val xml = cleanedUp map Source.fromString map (ConstructingParser.fromSource(_, preserveWS = false).document())
    val parsed = parse(xml)
    parsed
  }

  type Point2d = (Double, Double)

  def toInt(value: Double, precision: Double) = ((value + precision*.5)/precision).toInt

  case class Segment(topLeft: Point2d , originalWidth: Double, text: String) {
    def width = toInt(originalWidth, HorizontalPrecision)
    def top = topLeft._1
    def left = topLeft._2
    def rowNumber    = toInt(top, VerticalPrecision)
    def columnNumber = toInt(left, HorizontalPrecision)
  }

  def buildSegment(style: String, width: String, text: String): Result[Segment] = {
    // a typical style looks like this:
    //style="font-size: 10.72px; font-family: serif; left: 27.84px; top: 140.96px; transform: rotate(0deg) scale(1.15535, 1); transform-origin: 0% 0% 0px;"
    // we convert it to pairs like ("left", "27.84px")
    val styleKeyValuePairs: Array[List[String]] = style split ";" map (_ split ":" toList)

    val styleMap = styleKeyValuePairs collect { case key::value::Nil ⇒ key.trim->value.trim } toMap

    def property(key: String): Result[String] = Result(styleMap get key, s"$key missing in $styleMap")

    val PX = "(\\d*\\.?\\d*)px".r
    def doubleValueOf(key: String): Result[Double] = property(key) collect(
      {case PX(x) if !x.isEmpty ⇒ x.toDouble},
      "Could not extract double from " + _
    )

    doubleValueOf("top") andAlso doubleValueOf("left") map (Segment(_, width.toDouble, text)) orElse Empty
  }

  private def tabulate(tabs: List[Int]) = (segs: List[Segment]) ⇒ {
    val segMap = segs .map (s ⇒ s.columnNumber -> s.text) .toMap withDefaultValue ""
    tabs map segMap
  }

  def chunk(seg: Segment) = Chunk(seg.text, seg.columnNumber, seg.width)

  private def pageOf(rows: Map[Int, List[Segment]]): Page = {
    val orderedLines = rows.keySet.toList.sorted map rows
    val page = Page(DefaultDocumentBuilder, None, orderedLines map (_ map chunk))
    page
  }

  def parsePage(html: NodeSeq): Result[Page] = {
    val extracted: Set[Result[Segment]] = (html \\ "div") collect {
      case div if div.label == "div" ⇒
        val style: String = (div \ "@style").text
        val text = div.child.toString
        val width: String = (div \ "@data-canvas-width").text
        val direction: String = (div \ "@data-data-angle").text
        val seg = buildSegment(style, width, text)
        seg
      case otherwise ⇒
        Empty
    } filter(_.nonEmpty) toSet

    val segmentsOpt:Result[List[Segment]] = Result.traverse(extracted).map(_.toList)

    val result = for (segments <- segmentsOpt) yield {
      val rows:Map[Int, List[Segment]] = segments .groupBy(_.rowNumber)
      val sortedRows = rows.mapValues ((row:List[Segment]) ⇒ row.sortBy(_.columnNumber))
      pageOf(sortedRows)
    }
    result match {
      case Good(page) ⇒ Good(page)
      case x if x.isEmpty      ⇒ Good(Page.empty)
      case bad        ⇒ bad
    }
  }

  def parse(html: List[NodeSeq]): Result[SimpleTextDocument] = {
    val optPages = html map parsePage
    val docOpt = for (pages <- Result.traverse(optPages)) yield new SimpleTextDocument(DefaultDocumentBuilder, pages.toList)
    docOpt
  }
}
