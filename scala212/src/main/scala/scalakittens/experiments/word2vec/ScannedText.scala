package scalakittens.experiments.word2vec

/**
  * Represents scanned text - contains 
  * - inverse index
  * - list of words, by frequency, increasing
  * - list of frequencies
  * - word positions in the original text
  * 
  * Created by vpatryshev on 5/12/17.
  */
case class ScannedText(inverseIndex: Map[String, List[Int]],
                       words: List[String],
                       frequencies: List[Int]) {
  
  lazy val positions: List[Int] = words flatMap inverseIndex

}
