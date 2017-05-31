package scalakittens.experiments.word2vec

import Sigma._
import scalakittens.la._

/**
  * Created by vpatryshev on 5/11/17.
  */
case class SkipGramModel(text: ScannedText, dim: Int, α: Double, window: Int, numEpochs: Int, seed: Long = System.nanoTime()) {
  require (0 < α && α < 1.0/dim, s"α=$α should be between 0 and ${1.0/dim}")
  
  lazy val vecFactory = Vector.RandomSphere(dim, seed)

  lazy val in: Array[Vector] = {
    val in = new Array[Vector](text.dictionarySize)

    for {i <- in.indices} in(i) = vecFactory()
    
    in
  }

  def checkme(msg: String = "", which: Int = -1): Unit = {
    in.indices.foreach(i => {
      val v = in(i)

      if (which < 0 || i == which)
      v.foreach(
        xi => {
          require(!xi.isNaN, s"Vector at $i; $msg")
          ()
        })})
      
    require(in.length == 17694, s"Actually ${in.length}")
  }
  
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
      val prox: Double = proximity(i, abs(j))
      val g = α * ((if (j < 0) 0 else 1) - prox)
      if (g != 0) {
        neu.nudge(w, g)
        w.nudge(v, g)
        require(w.isValid, s"w is bad:\nw=$w,\nv=$v,\ng=$g")
      }
      
    }
 
    v += neu
    require(v.isValid, s"3. vector at $i is bad: $v")
    ()
  }
  
  def updateWindow(i: Int): Unit = {
    val idx = text.index(i)
    val v: Vector = in(text.index(i))

    require(v.isValid, s"in vector at $i -> $idx is bad: $v")

    val limit = text.length
    for {j <- max(0, i - window) until min(limit, i+window) if j != i} {
      require(v.isValid, s"in vector at $i -> $idx is bad: $v")
      update(idx, text.index(j))
    }
  }
  
  def doOneEpoch(): Unit = {
    0 until text.length foreach { i => 
      updateWindow(i)
    } 
  }
  
  def run(): Unit = {
    (0 until numEpochs).foreach(i => {
      print(s"-sg$i-")
      doOneEpoch()
    })
  }
}
