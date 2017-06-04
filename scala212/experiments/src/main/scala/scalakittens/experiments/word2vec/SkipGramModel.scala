package scalakittens.experiments.word2vec

import Sigma._
import scalakittens.la._

/**
  * Created by vpatryshev on 5/11/17.
  */
case class SkipGramModel(text: ScannedText, dim: Int, α: Double, window: Int, numEpochs: Int, seed: Long = System.nanoTime()) {

  require (0 < α && α < 1.0/dim, s"α=$α should be between 0 and ${1.0/dim}")
  
  lazy val vecFactory = Vector.RandomSphere(dim, seed)

  lazy val in: Array[MutableVector] = {
    val in = new Array[MutableVector](text.dictionarySize)

    for {i <- in.indices} in(i) = vecFactory().copy
    
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
  
  val out: Array[MutableVector] = new Array[MutableVector](huffman.size)

  for {i <- out.indices} out(i) = vecFactory().copy

  def product(i: Int, j: Int) = in(i) * out(j)
  
  def proximity(i: Int, o: Int) = σ(product(i, o))

  import math._

  def update(i: Int, o: Int): Unit = {
    val v = in(i)
    val neu: MutableVector = Vector.Zero(dim).copy
  
    for {j <- huffman.path(o)} {
      val w = out(abs(j))
      val prox: Double = proximity(i, abs(j))
      val g = α * ((if (j < 0) 0 else 1) - prox)
      if (g != 0) {
        neu.nudge(w, g)
        w.nudge(v, g)
      }
      
    }
 
    v += neu
    ()
  }
  
  def updateWindow(i: Int): Unit = {
    val idx = text.index(i)

    val limit = text.length
    for {j <- max(0, i - window) until min(limit, i+window) if j != i} {
      update(idx, text.index(j))
    }
  }
  
  def doOneEpoch(numEpoch: Int): Unit = {
    val numCores = 4//Runtime.getRuntime.availableProcessors
//    val t0 = System.currentTimeMillis
//    println(s"Epoch $numEpoch on $numCores cores")
    val oneRange = text.length / numCores
    val threads = (0 until numCores).par
    threads.foreach((t:Int) => {
      val range = (t * oneRange) until min(text.length, (t+1)*oneRange)
//      print(s"(t${Thread.currentThread().getId}: b$t) ")
      range foreach updateWindow
    })
//    println(s"\nDone epoch $numEpoch, ${System.currentTimeMillis - t0}")
  }
  
  def run(): Unit = {
    doOneEpoch(0)
    val t0 = System.currentTimeMillis
    (1 until numEpochs).foreach(i => {
      val t1 = System.currentTimeMillis
      doOneEpoch(i)
      println(s"$i: ${System.currentTimeMillis - t1}")
    })
    println(s"Average: ${(System.currentTimeMillis - t0)*1.0/(numEpochs - 1)}")
    
  }
}
