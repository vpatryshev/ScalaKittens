package org.scalakittens.web.common.parsing.pdf

import java.io._
import java.util
import java.util.Map.Entry

import org.scalakittens.web.common.parsing.pdf.SimpleTextDocument.DefaultDocumentBuilder
import org.scalakittens.web.common.Good
import org.apache.pdfbox.cos._
import org.apache.pdfbox.pdfparser.PDFParser
import org.apache.pdfbox.util.{PDFText2HTML, PDFTextStripper}
import org.scalakittens.{OS, Result}

import scala.collection.JavaConversions._
import scala.io.Source
import scala.language.postfixOps
import scala.xml.XML

/*
 * PDF functionality, using pdfbox
 */
trait PDF {
  val separator: String = "|"

  class PdfSource(in: InputStream, fromPage: Int = 1, toPage: Int = 0/*all*/) {
    def from(page: Int) = new PdfSource(in, page, toPage)
    def to(page: Int)   = new PdfSource(in, fromPage, page)

    def read(extractor: PDFTextStripper) = {
      val parser = new PDFParser(in)
      parser.parse()
      val doc = parser.getPDDocument
      val out = new StringWriter

      try {
        extractor.setStartPage(fromPage)
        if (toPage > fromPage - 1) extractor.setEndPage(toPage)
        extractor.writeText(doc, out)
        doc.close()
        out.toString
      } finally {
        in.close()
      }
    }
  }

  /**
   *  Extracts pdf into a primitive html
   */
  def pdfToHTML(in: InputStream, startPage:Int = 1, endPage: Int = 0/*all*/)
               (implicit converter: PDFText2HTML = new PDFText2HTML("ASCII")): String = {
    val raw = new PdfSource(in) from startPage to  endPage read converter
    // now drop the faulty doctype that pdf produces
    raw.split     ("\n") // now we have lines
       .dropWhile (!_.contains("<html>")) // ignoring the leading stuff that is wrong
       .filter    (!_.startsWith("<meta")) // meta is wrong too
       .mkString  ("\n")
  }

  def htmlWithFragmentSeparators: PDFText2HTML = {
    new PDFText2HTML("ASCII") {
      override def writeString(s: String) {
        super.writeString(s + separator)
      }
    }
  }

  /**
   *  Extracts pdf into one string; line separators from text mixed with those that come from element separators
   */
  def pdfToString(in: InputStream, startPage:Int = 1, endPage: Int = 0/*all*/): String =
    new PdfSource(in) from startPage to  endPage read new PDFTextStripper

  def extractText(pdf: File, startPage:Int = 1, endPage: Int = 0/*all*/): SimpleTextDocument =
    extractTextFrom(new FileInputStream(pdf), startPage, endPage)

  val DpiForRendering = 1200

  def extractTextFromImagesHiddenInPdf(file: File): Result[String] = {
    val pdf = Good(file).
      filter (_.exists, f ⇒ s"File $f does not exist").
      filter (_.canRead, f ⇒ s"File $f unreadable").
      filter (_.length > 100, f ⇒ s"File $f too short (${f.length} bytes)").
      map(_.getAbsoluteFile.getCanonicalFile)
    val txt = pdf map (f ⇒ new File(f.getAbsoluteFile.getCanonicalPath.replace(".pdf", ".txt")))
    val fileExists:Result[File] = txt.filter(_.canRead, f ⇒ {
      s"Failed to create file $f"
    })
    val fileRetrieved:Result[String] = fileExists.map(_.getPath) orElse pdf flatMap (OS.exec("./ocrpdf.sh", _))
    fileRetrieved map (Source.fromFile(_).mkString)
  }

  /**
   *  Extracts pdf into a list of pages; each page is a list of strings as represented in source document (may contain line separators)
   */
  def extractTextFrom(in: InputStream, startPage:Int = 1, endPage: Int = 0/*all*/): SimpleTextDocument =
    PDF.newDocument(parseWithFragmentSeparators(in, startPage, endPage))

  def parseWithFragmentSeparators(in: InputStream, startPage:Int = 1, endPage: Int = 0/*all*/): String = {
    pdfToHTML(in, startPage, endPage)(htmlWithFragmentSeparators)
  }

  // this class is work in progress; so far could not extract any meaningful structure out of pdf; please ignore it - or fix it :)
  class PdfContent(doc: COSDocument) {
    def objects = doc.getObjects.toArray collect {case o: COSObject ⇒ o}
    lazy val contents = objects.head

    class PrintVisitor(out: Writer, margin: String = "") extends ICOSVisitor {

      def deeper = new PrintVisitor(out, margin + "  ")

      def log(is: InputStream) {
        Source.fromInputStream(is).getLines() foreach log
      }

      def log(x: Any):Unit = {out.write(margin + x + "\n")}
      def debug(x: Any) = {out.write("DEBUG: " + x + "\n")}

      def visitAny(cos: COSBase) = {
        log("**Visiting " + (
          Option(cos) match {
          case Some(c) ⇒ s"${c.getClass} $c"
          case None ⇒ "nothing"
        }))
        cos
      }

      def visitFromArray(p1: COSArray): AnyRef = {
        log("--ARRAY, SIZE=" + p1.size + "--")

        for (i ← 0 until p1.size) {
          val rawName = Option(p1.getName(i))
          val name = rawName.fold("-")(_.replaceAll("[~ -z]", "?!?!"))
          log("[" + i + "] " + name)
          p1.get(i).accept(deeper)
        }
        p1
      }

      def visitFromBoolean(p1: COSBoolean): AnyRef = visitAny(p1)

      val toSkip = Set("Parent", "MediaBox", "Type", "H", "L", "O", "N", "T", "Resources")
      def skipThisObject(name: COSName) =
        Option(name) filter (toSkip contains _.getName) isEmpty


      def isGoodDictionary(d: COSDictionary): Boolean = {
        val keys: Set[String] = d.keySet.toSet map ((key:COSName) ⇒ key.getName)
        (keys filter (_.startsWith("Font"))).isEmpty
      }

      def visitFromDictionary(p1: COSDictionary): AnyRef = {
        if (!isGoodDictionary(p1)) log("...font: Skipping") else {
          log("--DICTIONARY--")
          val entrySet: util.Set[Entry[COSName, COSBase]] = Some(p1.entrySet()).getOrElse(util.Collections.emptySet())
          if (entrySet.isEmpty) log ("(no entries)")
          else {
            val i = entrySet.iterator()
            while (i.hasNext) {
              val x: Entry[COSName, COSBase] = i.next
              val name = x.getKey
              val doSkip: Boolean = skipThisObject(name)
              if (doSkip) {
                //log("..(" + name.getName + ")")
              } else {
                log(name.getName + "→")
                x.getValue.accept(deeper)
              }
            }
          }
        }
        p1
      }

      def visitFromDocument(p1: COSDocument): AnyRef = {
        visitAny(p1)
        p1.accept(this)
      }

      def visitFromFloat(p1: COSFloat): AnyRef = {
        log("Float=" + p1.doubleValue())
        p1
      }

      def visitFromInt(p1: COSInteger): AnyRef = {
        try {
          log("Integer=" + p1.longValue())
        } catch {
          case x: Exception ⇒ log("Integer(none)")
        }
        p1
      }

      def visitFromName(p1: COSName): AnyRef = visitAny(p1)

      def visitFromNull(p1: COSNull): AnyRef = visitAny(p1)

      def visitFromStream(p1: COSStream): AnyRef = {
        log("Got Stream, skipping!!!!!")
//        deeper.log(p1.getUnfilteredStream)
        p1
      }

      def visitFromString(p1: COSString): AnyRef = visitAny(p1)
    }

    def stringify(o: COSObject) = {
      val out = new StringWriter()
      val printVisitor: ICOSVisitor = new PrintVisitor(out)
      out.write("Object#" + o.getObjectNumber)
      o.accept(printVisitor)
      out.toString
    }

    override def toString = stringify(contents)
  }

  // work in progress
  def pdfContent(in: InputStream): PdfContent = {
    try {
      val parser = new PDFParser(in)
      parser.parse()
      val doc = parser.getDocument
      new PdfContent(doc)
    } finally {
      in.close()
    }
  }
}

object PDF extends PDF {
  val splitter = (s: String) ⇒ s split "\\|" toList

  def newDocument(s: String) = DefaultDocumentBuilder.buildDocument(XML.loadString(s), splitter)
}


