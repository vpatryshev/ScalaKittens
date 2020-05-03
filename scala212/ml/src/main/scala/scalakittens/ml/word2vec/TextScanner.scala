package scalakittens.ml.word2vec

import scala.collection.mutable
import scalakittens.{Result, IO, Strings}

/**
  * Formalized representation of source of words
  * We need to provide some kind of reader (line iterator is ok)
  * We need a predicate that detects the beginning of the content
  * We need a predicate that detects the end of the context
  * We need a predicate that detects document separator
  * We need a predicate that checks whether we a word (and not a stop word)
  *
  * Created by vpatryshev on 5/11/17.
  *
  * TODO: use Lucene for tokenization
  */
trait TextScanner {
  val name: String = this.getClass.getSimpleName.toLowerCase.replaceAll("\\$", "")
  def source: Result[Iterator[String]] = IO.linesFromResource(s"/$name.txt")

  def scannedText: Result[ScannedText] = source map scan

  def isBeginning(line: String): Boolean

  def isEnd(line: String) = line contains "End of the Project Gutenberg EBook"

  def isDocumentSeparator(line: String): Boolean = line matches "\\s*CHAPTER [CILVX]+\\s*"

  def isWord(w: String) = w.length > 1 && w.matches("^[a-z].*") && !Strings.isStop(w)

  def extractContentLines(text: Iterator[String]): Iterator[String] = text dropWhile (!isBeginning(_)) takeWhile (!isEnd(_)) filterNot isDocumentSeparator

  def wordStream(source: Iterator[String]): Iterator[String] = {
    for {
      line <- extractContentLines(source)
      l = Strings.normalize(line)
      word <- l.split(" ") filter isWord
    } yield word
  }

  def scan(source: Iterator[String]): ScannedText = {
    
    val inverseIndex = new mutable.HashMap[String, List[Int]] withDefaultValue Nil

    var i: Int = -1
    wordStream(source) foreach {
      w ⇒ {
        i+= 1
        inverseIndex(w) = i::inverseIndex(w)
      }
    }

    require(inverseIndex.size > 1, s"Weird text, empty inverse index; added ${i+1} words actually")

    val frequencies = inverseIndex.toList.map{case (w,l) ⇒ (w, l.size)}.sortBy(_._2).zipWithIndex

    require(frequencies.size > 1, s"Weird text, 0 frequencies")

    val words: List[String] = frequencies map (_._1._1)

    val freq: List[Int] = frequencies map (_._1._2)

    ScannedText(inverseIndex.toMap mapValues(_.reverse), words, freq)
  }
}
