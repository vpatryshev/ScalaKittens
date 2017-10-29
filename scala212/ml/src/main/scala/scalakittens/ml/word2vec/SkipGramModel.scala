package scalakittens.ml.word2vec
  
import scalakittens.la._
import scalakittens.ml.word2vec.Sigmoid.σ

/**
  * 
  * @param text Scanned text that is undergoing SkipGram model processing
  * @param space the instance of space in which the vectors are
  * @param numEpochs number of iterations
  * @param α gradient descent speed parameter, must be positive and less than 1/space.dim
  * @param window
  * @param seed
  * @tparam Space the type of space in which the vectors are
  *               
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

  val huffman = new HuffmanTree(text.frequencies)
  
  val out: Array[space.MutableVector] = new Array[space.MutableVector](huffman.size)

  for {i <- out.indices} out(i) = vecFactory().copy

  // TODO: get rid of casting
  def product(i: Int, j: Int) = {
    out(j) * in(i).asInstanceOf[space.Vector]
  }
  
  def proximity(i: Int, o: Int) = σ(product(i, o))

  import math._

  /**
    * Update a vector representation of word numbers i and o (and its relatives)
    *
    * As a result, vector i is nudged towards vector o and its parents in the hierarchy;
    * vector o, together with its parents in the hierarchy are also nudged.
    * The degree at which vectors are nudged depend on their proximity; the closer, the more 
    * they are nudged towards each other.
    * 
    * @param i first word number 
    * @param o second word number
    */
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
  
  /**
    * Update vector representations of words (within a window) surrounding a word at position i.
    * 
    * @param i word position in text
    */
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