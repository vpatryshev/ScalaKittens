package scalakittens.ml.word2vec

import scalakittens.IO

/**
  * Created by vpatryshev on 10/29/17.
  */
case object GoneWithTheWind extends TextScanner {
  
  def isBeginning(line: String) = line matches "Scarlett O'Hara was not beautiful.*"
  override def isEnd(line: String) = line contains "THE END"
  override def isDocumentSeparator(line: String) = line matches "\\s*CHAPTER [CILVX]+\\s*"

}
