package scalakittens.ml.word2vec

import scalakittens.IO

/**
  * Created by vpatryshev on 10/29/17.
  */
case object WarAndPeace extends TextScanner {
  
  def isBeginning(line: String) = line matches "\\s*.{5,10}[Pp]rince.*"
  override def isEnd(line: String) = line contains "End of the Project Gutenberg EBook"
  override def isDocumentSeparator(line: String) = line matches "\\s*CHAPTER [CILVX]+\\s*"

}
