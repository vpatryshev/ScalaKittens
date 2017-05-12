package scalakittens.experiments.word2vec

import language.postfixOps
import scala.collection.mutable
import scalakittens.{Result, Good, Strings}

/**
  * Formailized representation of source of words
  * We need to provide some kind of reader (line iterator is ok)
  * We need a predicate that detects the beginning of the content
  * We need a predicate that detects the end of the context
  * We need a predicate that detects document separator
  * We need a predicate that checks whether we a word (and not a stop word)
  * 
  * Created by vpatryshev on 5/11/17.
  */
trait TextScanner {
  
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
  
  def scan(source: Iterator[String]): (Map[String, List[Int]], List[String], List[Int]) = {
    val inverseIndex = new mutable.HashMap[String, List[Int]] withDefaultValue Nil
    
    var i: Int = -1
    wordStream(source) foreach { w => { i+= 1; inverseIndex(w) = i::inverseIndex(w)}}
    
    val frequencies = inverseIndex.toList.map{case (w,l) => (w, l.size)}.sortBy(_._2).zipWithIndex

    val words: List[String] = {
      for {e: ((String, Int), Int) <- frequencies} yield e._1._1
    }

    val freq: List[Int] = {
      for {e: ((String, Int), Int) <- frequencies} yield e._1._2
    }
    
    (inverseIndex.toMap, words, freq)
  }
}

object TextScanner {
  val WarAndPeace = new TextScanner {
    def isBeginning(line: String) = line matches "\\s*.{5,10}[Pp]rince.*"
    override def isEnd(line: String) = line contains "End of the Project Gutenberg EBook"
    override def isDocumentSeparator(line: String) = line matches "\\s*CHAPTER [CILVX]+\\s*"
  }
}