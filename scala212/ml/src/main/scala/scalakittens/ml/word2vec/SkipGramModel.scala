package scalakittens.ml.word2vec
  
import scalakittens.la._
import scalakittens.ml.word2vec.Sigma._

/**
  * Created by vpatryshev on 5/11/17.
  */
case class SkipGramModel[Space <: VectorSpace](text: ScannedText, space: Space, α: Double, window: Int, numEpochs: Int, seed: Long = System.nanoTime()) {
  
  require (0 < α && α < 1.0/space.dim, s"α=$α should be between 0 and ${1.0/space.dim}")
  lazy val vecFactory = space.RandomSphere(seed)

  lazy val in: Array[Space#MutableVector] = {
    val in = new Array[Space#MutableVector](text.dictionarySize)

    for {i <- in.indices} in(i) = vecFactory().copy
    
    in
  }

  def checkme(msg: String = "", which: Int = -1): Unit = {
    in.indices.foreach(i => {
      val v: Space#Vector = in(i)

      if (which < 0 || i == which)
      v.foreach(
        xi => {
          require(!xi.isNaN, s"Vector at $i; $msg")
          ()
        })})
      
    require(in.length == 17694, s"Actually ${in.length}")
  }
  
  val huffman = new HuffmanTree(text.frequencies)
  
  val out: Array[space.MutableVector] = new Array[space.MutableVector](huffman.size)

  for {i <- out.indices} out(i) = vecFactory().copy

  // TODO: get rid of casting
  def product(i: Int, j: Int) = {
    out(j) * in(i).asInstanceOf[space.Vector]
  }
  
  def proximity(i: Int, o: Int) = σ(product(i, o))

  import math._

  def update(i: Int, o: Int): Unit = {
    val v = in(i).asInstanceOf[space.MutableVector] // TODO: get rid of casting
    val neu: space.MutableVector = space.Zero.copy
  
    for {j <- huffman.path(o)} {
      val w: space.MutableVector = out(abs(j))
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
    val numCores = Runtime.getRuntime.availableProcessors
    val oneRange = (text.length + numCores-1) / numCores
    val threads = (0 until numCores).par
    threads.foreach((t:Int) => {
      val range = (t*oneRange) until min(text.length, (t+1)*oneRange)
      range foreach updateWindow
    })
  }
  
  def run(): Unit = {
    doOneEpoch(0)
    val t0 = System.currentTimeMillis
    (1 until numEpochs).foreach(i => {
      val t1 = System.currentTimeMillis
      doOneEpoch(i)
      println(s"$i: ${System.currentTimeMillis - t1}")
    })
    println(s"Average: ${(0.5 + (System.currentTimeMillis - t0)*1.0/(numEpochs - 1)).toInt}")
    
  }
}

object SkipGramModel {

  def run(dim: VectorSpace, numEpochs: Int, α: Double, st: ScannedText) = {
    val model = SkipGramModel(st, dim, α, window = 3, numEpochs, seed = 123456789L)
    model.run()
    val originalVectors = model.in
    originalVectors
  }
}