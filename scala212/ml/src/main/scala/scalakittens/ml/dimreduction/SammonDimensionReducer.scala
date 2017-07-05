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

  protected val init: IndexedSeq[S#Vector] => IndexedSeq[target.Vector]
  
  class Iterations(originalVectors: IndexedSeq[S#Vector], magicFactor: Double = 0.35) {
    lazy val initialVectors = init(originalVectors)
    val dim = initialVectors.head.length
    val size = initialVectors.size
    val globalSpace = VectorSpace(size)
    type DistanceMatrix = globalSpace.SquareMatrix
    
    def distanceMatrix[Space <: VectorSpace](vs: IndexedSeq[Space#Vector]): DistanceMatrix = {
      val matrix = globalSpace.squareMatrix((i, j) => {
        val d = Norm.l2.distance(vs(i), vs(j))
        if (d.isNaN) throw new IllegalStateException(s"Bad distance matrix at $i, $j: d=$d, for vectors ${vs(i)} and ${vs(j)}")
        d
      })
      matrix.triangle
    }
    
    lazy val originalDistanceMatrix: DistanceMatrix = distanceMatrix(originalVectors).triangle

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
        val distance = m(p, j)
        val dpj_ = originalDistanceMatrix(p, j)
        val quotient: Double = (dpj_ - distance) / distance / dpj_
        val delta: target.Vector = vectors(p) - vectors(j) * quotient
        v += delta
      }
      v
    }

    def `d2E/dy2`(vectors: IndexedSeq[target.Vector], m: DistanceMatrix, p: Int): Int => Double = {
      val v = target.Zero.copy
      for {
        j <- 0 until size if j != p
      } {
        val dpj = m(p, j)
        val dpj_ = originalDistanceMatrix(p, j)

        val q1 = 1.0 / dpj_ / dpj
        val q2 = dpj_ / dpj / dpj
        
        v += new target.OnFunction(q => {
          val d = vectors(p)(q) - vectors(j)(q)
          q1 * (dpj_ - dpj - d*d*q2)
        }
        )
      }
      v
    }
    
    def Δ(vs: IndexedSeq[target.Vector], m: DistanceMatrix, p: Int): target.Vector = {
      val firstDerivative = `dE/dy`(vs, m, p)
      val secondDerivative = `d2E/dy2`(vs, m, p)
      val result = firstDerivative / (secondDerivative compose math.abs)
      
      if (!result.isValid) throw new IllegalStateException(s"Bad delta at $p: $result; first Derivative = $firstDerivative, second derivative = $secondDerivative")

      result
    }

    def step(i: Int, vectors: IndexedSeq[target.Vector]): (IndexedSeq[target.Vector], DistanceMatrix) = {
      println(s"--> Step $i")
      val matrix = distanceMatrix(vectors)
      val newVectors = vectors.indices map { p => {
//        if (p > 5) vectors(p) else 
        {
          val d = Δ(vectors, matrix, p)
          val v = vectors(p) - d * magicFactor
          if (!v.isValid) throw new IllegalStateException(s"Bad vector$p in step $i: $v, where original is ${vectors(p)} and d=$d")
          v
        }
      }}
      (newVectors, matrix)
    }
    
    def run():  IndexedSeq[target.Vector] = {
      
      val result = (initialVectors /: (1 to numSteps)){(vs, i) => {
        val t0 = System.currentTimeMillis
        val (newVs, mx) = step(i, vs)
        val t1 = System.currentTimeMillis
        val err = error(mx)
        val t2 = System.currentTimeMillis
        println(s"Step $i, error = $err; t1=${t1-t0}, t2=${t2-t1}")
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