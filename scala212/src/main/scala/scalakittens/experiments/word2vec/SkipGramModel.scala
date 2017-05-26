package scalakittens.experiments.word2vec

import Sigma._
import scalakittens.la._

/**
  * Created by vpatryshev on 5/11/17.
  */
case class SkipGramModel(text: ScannedText, dim: Int, α: Double, window: Int, numEpochs: Int, seed: Long = System.nanoTime()) {
  val in: Array[Vector] = new Array[Vector](text.dictionarySize)
  val vecFactory = Vector.RandomSphere(dim, seed)
  
  for {i <- in.indices} in(i) = vecFactory()

  val huffman = new HuffmanTree(text.frequencies)
  
  val out: Array[Vector] = new Array[Vector](huffman.size)

  for {i <- out.indices} out(i) = vecFactory()

  def product(i: Int, j: Int) = in(i) * out(j)
  
  def proximity(i: Int, o: Int) = σ(product(i, o))

  import math._

  def update(i: Int, o: Int): Unit = {

    val v = in(i)
    val neu = Vector.Zero(dim)()
  
    for {j <- huffman.path(o)} {
      val w = out(abs(j))
      val g = α * ((if (j < 0) 0 else 1) - proximity(i, abs(j)))
      neu.nudge(w, g)
      w.nudge(v, g)
    }
  
    v += neu
    ()
  }
  
  def updateWindow(i: Int): Unit = {
    for {j <- max(0, i - window) until min(text.length, i+window) if j != i} {
      update(text.index(i), text.index(j))
    }
  }
  
  def doOneEpoch(): Unit = {
    0 until text.length foreach updateWindow 
  }
  
  def run(): Unit = {
    (0 until numEpochs).foreach(i => {
      print(s"-sg$i-")
      doOneEpoch()
    })
  }
}
