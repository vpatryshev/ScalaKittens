package scalakittens.ml.dimreduction

import scala.annotation.tailrec
import scala.language.postfixOps
import scalakittens.la._

/**
  * Sammon algorithm for dimension reduction
  *
  * @see [[http://theoval.cmp.uea.ac.uk/~gcc/matlab/sammon/sammon.pdf]]
  * @see [[https://en.wikipedia.org/wiki/Sammon_mapping]]
  * @see [[https://link.springer.com/article/10.1007%2Fs100440050006]] - why you need PCA as initialization
  *
  *      Created by vpatryshev on 6/11/17.
  */
abstract class SammonDimensionReducer[S <: VectorSpace, T <: VectorSpace](
                                                                           val source: S,
                                                                           val target: T,
                                                                           val numSteps: Int
                                                                         ) extends DimensionReducer[S#Vector, T#Vector] {
  type Target = T

  val dim = source.dim

  protected val init: IndexedSeq[S#Vector] => IndexedSeq[target.Vector]

  def reduce(originalVectors: IndexedSeq[S#Vector]) = {
    val iterations = new Iterations(originalVectors)
    //    println(s"Will run on ${originalVectors.length} vectors")
    iterations.run()
  }

  class Iterations(originalVectors: IndexedSeq[S#Vector], magicFactor: Double = 0.01) {
    type DistanceMatrix = globalSpace.SquareMatrix
    lazy val initialVectors = init(originalVectors)
    lazy val maxNorm = 10 * initialVectors.map(Norm.l2(_)).max
    lazy val originalDistanceMatrix: DistanceMatrix = distanceMatrix(globalSpace, originalVectors).triangle
    lazy val summaryNorm = Norm.l2(originalDistanceMatrix) / 2
    val size = originalVectors.size
    val globalSpace = VectorSpace(size)

    def distanceMatrix[Space <: VectorSpace](space: Space, vs: IndexedSeq[Space#Vector]): DistanceMatrix = {
      val matrix = globalSpace.squareMatrix((i, j) => {
        val d = Norm.l2.distance(vs(i), vs(j))
        if (d.isNaN) throw new IllegalStateException(s"Bad distance matrix at $i, $j: d=$d, for vectors ${vs(i)} and ${vs(j)}")
        d
      })
      matrix.triangle
    }

    def error(matrix: globalSpace.SquareMatrix) = {
      (0.0 /: (0 until size * size)) {
        (s, ij) =>
          val i = ij / size
          val j = ij % size
          if (j >= i) s else {
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
        if (math.abs(dpj) > 1 / maxNorm) {
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
      if (!gradient.isValid) throw new IllegalStateException(s"Bad gradient: $gradient, for p=$p")
      val stepSize = Norm.l2(gradient)
      if (stepSize > maxNorm) gradient * (maxNorm / stepSize) else gradient
    }

    def shift(i: Int, vectors: IndexedSeq[target.Vector], matrix: DistanceMatrix): IndexedSeq[target.Vector] = {
      vectors.indices map {
        p => {
          val d = Δ(vectors, matrix, p)
          if (!d.isValid) throw new IllegalStateException(s"Bad vector$p in step $i: $d, where original is ${vectors(p)} and d=$d")
          d
        }
      }
    }

    def run(): IndexedSeq[target.Vector] = {
      val m0 = distanceMatrix(globalSpace, initialVectors)
      val err0 = error(m0)
      val result = recurse(initialVectors, err0, m0, 0, numSteps, magicFactor)
      result._1
    }

    @tailrec private def recurse(vs: IndexedSeq[target.Vector], err0: Double, mx0: DistanceMatrix, i: Int, numSteps: Int, factor: Double): (IndexedSeq[target.Vector], Double, DistanceMatrix) = {
      if (i >= numSteps || factor < 0.00001) (vs, err0, mx0) else {
        val t0 = System.currentTimeMillis

        val step = shift(i, vs, mx0) map (_ * factor)

        val t1 = System.currentTimeMillis
        val newVs = (vs zip step).map { case (v, d) => v - d }
        Viz.visualize("Horizontal", newVs map { v => ("*", v.apply(0), v.apply(1)) })
        val mx1 = distanceMatrix(globalSpace, newVs)
        val t2 = System.currentTimeMillis
        val err = error(mx1)
        val t3 = System.currentTimeMillis

        val delta = step map (Norm.l2(_)) sum

        //        println(s"Step $i, factor=$factor, error = $err; delta = $delta; t1=${t1 - t0}, t2=${t2 - t1}, , t3=${t3 - t2}")

        if (err < err0 * 0.999) {
          recurse(newVs, err, mx1, i + 1, numSteps, factor)
        } else {
          recurse(vs, err0, mx0, i + 1, numSteps, factor * 0.7)
        }
      }
    }
  }

}

object SammonDimensionReducer {
  def withPCA[S <: VectorSpace, T <: VectorSpace](source: S, target: T, numIterations: Int) = {
    val pca = pcaReducer(source, target, numIterations)
    val reducer: DimensionReducer[S#Vector, T#Vector] = new SammonDimensionReducer[S, T](source, target, numIterations) {
      protected val init: IndexedSeq[S#Vector] => IndexedSeq[target.Vector] = v => pca.reduce(v).map(_.asInstanceOf[target.Vector])
    }

    reducer
  }

  private def pcaReducer[S <: VectorSpace, T <: VectorSpace](source: S, target: T, numIterations: Int) =
    new PcaDimensionReducer[S, T](source, target, precision = 0.001, numIterations)
}