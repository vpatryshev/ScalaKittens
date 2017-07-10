package scalakittens.ml.dimreduction

import scalakittens.la._

/**
  * Sammon algorithm for dimension reduction
  * @see [[http://theoval.cmp.uea.ac.uk/~gcc/matlab/sammon/sammon.pdf]]
  * @see [[https://en.wikipedia.org/wiki/Sammon_mapping]]
  * @see [[https://link.springer.com/article/10.1007%2Fs100440050006]] - why you need PCA as initialization
  *     
  * Created by vpatryshev on 6/11/17.
  */
abstract class SammonDimensionReducer[S <: VectorSpace, T <: VectorSpace](
    val source: S, 
    val target: T,
    val numSteps: Int
   ) extends DimensionReducer[S#Vector, T#Vector] {
  type Target = T

  val dim = source.dim

  protected val init: IndexedSeq[S#Vector] => IndexedSeq[target.Vector]
  
  class Iterations(originalVectors: IndexedSeq[S#Vector], magicFactor: Double = 0.1) {
    lazy val initialVectors = init(originalVectors)
    lazy val maxNorm = 10 * initialVectors.map(Norm.l2(_)).max
    val size = originalVectors.size
    val globalSpace = VectorSpace(size)
    type DistanceMatrix = globalSpace.SquareMatrix
    
    def distanceMatrix[Space <: VectorSpace](space: Space, vs: IndexedSeq[Space#Vector]): DistanceMatrix = {
      val matrix = globalSpace.squareMatrix((i, j) => {
        val d = Norm.l2.distance(vs(i), vs(j))
        if (d.isNaN) throw new IllegalStateException(s"Bad distance matrix at $i, $j: d=$d, for vectors ${vs(i)} and ${vs(j)}")
        d
      })
      matrix.triangle
    }
    
    lazy val originalDistanceMatrix: DistanceMatrix = distanceMatrix(globalSpace, originalVectors).triangle

    lazy val summaryNorm = Norm.l2(originalDistanceMatrix) / 2
    
    def error(matrix: globalSpace.SquareMatrix) = {
      (0.0 /: (0 until size*size)) {
        (s, ij) =>
          val i = ij/size
          val j = ij%size
          if (j >= i) s else {
            if (j == 0 && (i%1000 == 0)) println(s"($i,$j)")
            val dij_ = originalDistanceMatrix(i, j)
            val dij = matrix(i, j)
            val d = dij - dij_
            val delta = d * d / dij_
            val newS = s + delta
            if (newS.isNaN) {
              throw new IllegalStateException(s"Bad error, at ($i, $j): dij=$dij, dij=$dij_, d=$d")
            }
            if (math.abs(newS) < Double.MinPositiveValue * 1000) {
              throw new IllegalStateException(s"How come we have zero error?! i=$i, j=$j, d=$d, dij_ = $dij_, dij=$dij, delta=$delta")
            }
            newS
          }
      }
    } 
    
    def `dE/dy`(vectors: IndexedSeq[target.Vector], m: DistanceMatrix, p: Int): target.Vector = {
      val v: target.MutableVector = target.Zero.copy
      for {
        j <- 0 until size if j != p
      } {
        val dpj = m(p, j)
        if (math.abs(dpj) > 1/maxNorm) {
          val dpj_ = originalDistanceMatrix(p, j)
          val quotient: Double = (dpj - dpj_) / dpj
          val delta: target.Vector = vectors(p) - vectors(j)
//          if (!delta.isValid) throw new IllegalStateException(s"Invalid delta while calculating dE/dy: delta=$delta, quotent=$quotient, j=$j, p=$p, dpj = $dpj, dpj=${dpj_}")
//          if (Norm.l2(delta)*magicFactor > maxNorm) throw new IllegalStateException(s"Too big delta while calculating dE/dy: delta=$delta, quotent=$quotient, j=$j, p=$p, djp = $dpj, dpj=${dpj_}")
          v += delta * quotient
        }
      }
      v
    }
    
    def Δ(vs: IndexedSeq[target.Vector], m: DistanceMatrix, p: Int): target.Vector = {
      val gradient = `dE/dy`(vs, m, p)
      if (!gradient.isValid) throw new IllegalStateException(s"Bad first derivative: $gradient, for p=$p")
      
      if (!gradient.isValid) throw new IllegalStateException(s"Invalid delta at $p: ${gradient}; first Derivative = $gradient")
      val stepSize = Norm.l2(gradient)
      if (stepSize > maxNorm) gradient * (maxNorm / stepSize) else gradient
    }

    def shift(i: Int, vectors: IndexedSeq[target.Vector], matrix: DistanceMatrix): IndexedSeq[target.Vector] = {
      println(s"--> Step $i")
      val newVectors = vectors.indices map { p => {
          val d = Δ(vectors, matrix, p)
          if (!d.isValid) throw new IllegalStateException(s"Bad vector$p in step $i: $d, where original is ${vectors(p)} and d=$d")
          d
      }}
      newVectors
    }

    def step(i: Int, vectors: IndexedSeq[target.Vector]): (IndexedSeq[target.Vector], DistanceMatrix) = {
      println(s"--> Step $i")
      val matrix = distanceMatrix(globalSpace, vectors)
      val newVectors = vectors.indices map { p => {
        val d = Δ(vectors, matrix, p)
        val v = vectors(p) - d * magicFactor
        if (!v.isValid) throw new IllegalStateException(s"Bad vector$p in step $i: $v, where original is ${vectors(p)} and d=$d")
        v
      }}
      (newVectors, matrix)
    }
    
    def pub(vs: IndexedSeq[target.Vector]) = vs map (_.toString) mkString "\n"
    
    def run():  IndexedSeq[target.Vector] = {
      val m0 = distanceMatrix(globalSpace, initialVectors)
      val err0 = error(m0)
      val result = ((initialVectors, err0, m0) /: (1 to numSteps)){case ((vs, err0, mx0), i) => {
        val t0 = System.currentTimeMillis
        
        val t1 = System.currentTimeMillis
        val delta = shift(i, vs, mx0)
        vs.zip(delta)        
        val v1 = (vs zip delta) map (_ + _)
        val err = error(mx1)
        if (err > err0) newVs = (vs + newVs)/2
        val t2 = System.currentTimeMillis
        val delta = vs zip newVs map {case (x,y) => Norm.l2.distance(x,y)} sum
        
        println(s"Step $i, error = $err; delta = $delta; t1=${t1-t0}, t2=${t2-t1}")
        println(pub(vs))
        println(pub(newVs))
        newVs
        }
      }

      result
    }
  }
  
  def reduce(originalVectors: IndexedSeq[S#Vector]) = {
    val iterations = new Iterations(originalVectors)
    println(s"Will run on ${originalVectors.length} vectors")
    iterations.run()
  }
}

object SammonDimensionReducer {
  private def pcaReducer[S <: VectorSpace, T<: VectorSpace](source: S, target: T, numIterations: Int) =
    new PcaDimensionReducer[S, T](source, target, precision = 0.001, numIterations)

  def withPCA[S <: VectorSpace, T <: VectorSpace](source: S, target: T, numIterations: Int) = {
    val pca = pcaReducer(source, target, numIterations)
    val reducer: DimensionReducer[S#Vector, T#Vector] = new SammonDimensionReducer[S, T](source, target, numIterations) {
      protected val init: IndexedSeq[S#Vector] => IndexedSeq[target.Vector] = v => pca.reduce(v).map(_.asInstanceOf[target.Vector])
    }

    reducer
  }
}