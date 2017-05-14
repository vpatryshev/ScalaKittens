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
                       dictionary: List[String],
                       frequencies: List[Int]) {
  lazy val dictionarySize = dictionary.length
  lazy val length = index.length
  
  lazy val index: Array[Int] = {
    val size = inverseIndex.values.map(_.max).max + 1
    val index = new Array[Int](size)
    for {
      i <- dictionary.indices
      w = dictionary(i)
      j <- inverseIndex(w)
    } { index(j) = i }
    
    index
  }

}
