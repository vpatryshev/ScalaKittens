package scalakittens.ml.dimreduction

import language.postfixOps
import scalakittens.la._

/**
  * Sammon algorithm for dimension reduction
  * @see [[http://theoval.cmp.uea.ac.uk/~gcc/matlab/sammon/sammon.pdf]]
  * @see [[https://en.wikipedia.org/wiki/Sammon_mapping]]
  *     
  * Created by vpatryshev on 6/11/17.
  */
class SammonDimensionReductor(
    val sourceDim: Int, 
    val targetDim: Int,
    val numSteps: Int,
    val init: IndexedSeq[Vector] => IndexedSeq[Vector]
   ) extends DimensionReductor {

  class Iterations(originalVectors: IndexedSeq[Vector], magicFactor: Double = 0.35) {
    lazy val initialVectors = init(originalVectors)
    val dim = initialVectors.head.length
    val size = initialVectors.size

    def distanceMatrix(vs: IndexedSeq[Vector]): Matrix = {
      val matrix = new Matrix.OnFunction(size, size, (i, j) => {
        val d = Norm.l2.distance(vs(i), vs(j))
        if (d.isNaN) throw new IllegalStateException(s"Bad distance matrix at $i, $j: d=$d, for vectors ${vs(i)} and ${vs(j)}")
        d
      })
      matrix
      matrix.triangle
    }
    
    lazy val originalDistanceMatrix: Matrix = distanceMatrix(originalVectors).triangle

    lazy val summaryNorm = Norm.l2(originalDistanceMatrix) / 2
    
    def error(matrix: Matrix) = {
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
              throw new IllegalStateException(s"Bad error, at ($i, $j): dij=$dij, dij=${dij_}, d=$d")
            }
            if (math.abs(newS) < Double.MinPositiveValue * 1000) {
              throw new IllegalStateException(s"How come we have zero error?! i=$i, j=$j, d=$d, dij_ = $dij_, dij=$dij, delta=$delta")
            }
            newS
          }
      }
    } 
    
    def `dE/dy`(vectors: IndexedSeq[Vector], m: Matrix, p: Int): Vector = {
      val v = Vector.Zero(dim).copy
      for {
        j <- 0 until size if j != p
      } {
        val dpj = m(p, j)
        val dpj_ = originalDistanceMatrix(p, j)
        v += (vectors(p)-vectors(j)*((dpj_ - dpj)/dpj/dpj_))
      }
      v
    }

    def `d2E/dy2`(vectors: IndexedSeq[Vector], m: Matrix, p: Int): Int => Double = {
      val v = Vector.Zero(dim).copy
      for {
        j <- 0 until size if j != p
      } {
        val dpj = m(p, j)
        val dpj_ = originalDistanceMatrix(p, j)

        val q1 = 1.0 / dpj_ / dpj
        val q2 = dpj_ / dpj / dpj
        
        v += new Vector.OnFunction(dim, q => {
          val d = vectors(p)(q) - vectors(j)(q)
          q1 * (dpj_ - dpj - d*d*q2)
        }
        )
      }
      v
    }
    
    def Δ(vs: IndexedSeq[Vector], m: Matrix, p: Int): Vector = {
      val firstDerivative = `dE/dy`(vs, m, p)
      val secondDerivative = `d2E/dy2`(vs, m, p)
      val result = firstDerivative / (secondDerivative compose math.abs)
      
      if (!result.isValid) throw new IllegalStateException(s"Bad delta at $p: $result; first Derivative = $firstDerivative, second derivative = $secondDerivative")

      result
    }

    def step(i: Int, vectors: IndexedSeq[Vector]): (IndexedSeq[Vector], Matrix) = {
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
    
    def run():  IndexedSeq[Vector] = {
      
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
  
  def apply(originalVectors: IndexedSeq[Vector]): IndexedSeq[Vector] = {
    val iterations = new Iterations(originalVectors)
    println(s"Will run on ${originalVectors.length} vectors")
    iterations.run()
  }
}
