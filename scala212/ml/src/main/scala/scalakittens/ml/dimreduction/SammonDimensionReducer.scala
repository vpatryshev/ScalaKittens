package scalakittens.ml.dimreduction

import scala.annotation.tailrec
import scala.language.postfixOps
import scalakittens.la._
import ArrayOps._

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
  type MatrixLike = (Int, Int) => Double
  
  val dim = source.dim

  protected val init: IndexedSeq[S#Vector] => IndexedSeq[target.Vector]

  def reduce(originalVectors: IndexedSeq[S#Vector]) = {
    val iterations = new Iterations(originalVectors)
    //    println(s"Will run on ${originalVectors.length} vectors")
    iterations.run()
  }

  class Iterations(originalVectors: IndexedSeq[S#Vector], magicFactor: Double = 0.01) {
    type DistanceMatrix = globalSpace.TriangularMatrix
    lazy val initialVectors = init(originalVectors)
    lazy val maxNorm = 10 * initialVectors.map(Norm.l2(_)).max
    lazy val minDouble = {
      val d = 0.7
      val s = originalVectors.length
      d/s/s
    }
    lazy val originalDistanceMatrix: MatrixLike = distanceMatrix(globalSpace, originalVectors)
    
    def elementsOf(matrix: MatrixLike) = {
      val data = new Array[Double](globalSpace.dim * (globalSpace.dim - 1) / 2)
      for {
        i <- 0 until globalSpace.dim
        j <- 0 until i
      } {
        data(i*(i-1)/2 + j) = matrix(i, j)
      }
      data
    }
    
    lazy val originalDistanceMatrixData = elementsOf(originalDistanceMatrix)


    lazy val summaryNorm = Norm.l2(originalDistanceMatrixData) / 2
    val size = originalVectors.size
    val globalSpace = VectorSpace(size)

    def distanceMatrix[Space <: VectorSpace](space: Space, vs: IndexedSeq[Space#Vector]): MatrixLike = {
      
      val elementEvaluator: ((Int, Int) => Double) = 
          (i, j) => {
            Norm.l2.distance(vs(i), vs(j))
          }
      elementEvaluator
    }
    
    def error(matrix: MatrixLike) = {
      val thisData = elementsOf(matrix)
      val orgData = originalDistanceMatrixData
      val t0 = System.currentTimeMillis
      val e = sammonErrorMeasure.fold(thisData, orgData)
      println(s"Spent ${System.currentTimeMillis - t0} ms calculating error = $e")
      e
    }

    lazy val numberOfVerticesInUnitCube = 1 << target.dim

    private def vertexFor(i: Int): target.Vector = {
      val absNo = i % numberOfVerticesInUnitCube
      val d = minDouble
      val coordinates = (for {
        bitNo <- 0 until target.dim
      } yield if (((1 << bitNo) & absNo) == 0) d else -d) toArray
      
      new target.OnArray(coordinates)
    }
    
    private def stepInDirectionOf(i: Int) = {
      vertexFor(i)
    }

    def `dE/dy`(vectors: IndexedSeq[target.Vector], m: MatrixLike, p: Int): target.Vector = {
      val v: target.MutableVector = target.Zero.copy
      for {
        j <- 0 until size if j != p
      } {
        val dpj = m(p, j)
        val dpj_ = originalDistanceMatrix(p, j)

        val delta: target.Vector = if (math.abs(dpj) > minDouble) {
          (vectors(p) - vectors(j)) / dpj
        } else stepInDirectionOf(p)
        
        v += delta * (dpj - dpj_)
      }
      v
    }

    def Δ(vs: IndexedSeq[target.Vector], m: MatrixLike, p: Int): target.Vector = {
      val gradient = `dE/dy`(vs, m, p)
      if (!gradient.isValid) throw new IllegalStateException(s"Bad gradient: $gradient, for p=$p")
      val stepSize = Norm.l2(gradient)
      if (stepSize > maxNorm) gradient * (maxNorm / stepSize) else gradient
    }

    def shift(i: Int, vectors: IndexedSeq[target.Vector], matrix: MatrixLike): IndexedSeq[target.Vector] = {
      vectors.indices map {
        p => {
          val d = Δ(vectors, matrix, p)
          if (!d.isValid) throw new IllegalStateException(s"Bad vector$p in step $i: $d, where original is ${vectors(p)} and d=$d")
          d
        }
      }
    }

    def run(): IndexedSeq[target.Vector] = {
      val m0: MatrixLike = distanceMatrix(globalSpace, initialVectors)
      val err0 = error(m0)
      val result = recurse(initialVectors, err0, m0, 0, numSteps, magicFactor)
      result._1
    }

    @tailrec private def recurse(vs: IndexedSeq[target.Vector], err0: Double, mx0: MatrixLike, i: Int, numSteps: Int, factor: Double): (IndexedSeq[target.Vector], Double, MatrixLike) = {
      if (i >= numSteps || factor < 0.00001) (vs, err0, mx0) else {
        val t0 = System.currentTimeMillis

        val step = shift(i, vs, mx0)

        case class State(stepSize: Double) {
          lazy val newVectors = vs.indices map { i => vs(i) - step(i)*factor }
        }
        
        val t1 = System.currentTimeMillis
        println(s"#$i.1: ${t1-t0}ms")

        
        
        val newVs = vs.indices map { i => vs(i) - step(i)*factor } // !!! Use nudge
        val mx1 = distanceMatrix(globalSpace, newVs)
        val t2 = System.currentTimeMillis
        println(s"#$i.2: ${t2-t1}ms")
        val err = error(mx1)
        Viz.visualize(s"Sammon, step $i, err $err", newVs map { v => ("*", v.apply(0), v.apply(1)) })

        val t3 = System.currentTimeMillis
        val delta = step map (Norm.l2(_)) sum
        
        println(s"Step $i, factor=$factor, error = $err; delta = $delta; t1=${t1 - t0}, t2=${t2 - t1}, , t3=${t3 - t2}")

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