package scalakittens.ml.word2vec

/**
  * Created by vpatryshev on 10/29/17.
  */
case object GoneWithTheWind extends TextScanner {
  
  def isBeginning(line: String): Boolean = line matches "Scarlett O'Hara was not beautiful.*"
  override def isEnd(line: String): Boolean = line contains "THE END"
  override def isDocumentSeparator(line: String): Boolean = line matches "\\s*CHAPTER [CILVX]+\\s*"

}
