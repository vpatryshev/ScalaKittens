package scalakittens.ml.dimreduction

import scala.language.postfixOps
import scalakittens.la._
import ArrayOps._
import scalakittens.Tracker
import scalakittens.ml.GradientDescentEngine
import scalakittens.ml.GradientDescentEngine.Evaluator

/**
  * Sammon algorithm for dimension reduction
  *
  * @see [[http://theoval.cmp.uea.ac.uk/~gcc/matlab/sammon/sammon.pdf]]
  * @see [[https://en.wikipedia.org/wiki/Sammon_mapping]]
  * @see [[https://link.springer.com/article/10.1007%2Fs100440050006]] - why you need PCA as initialization
  *
  *      Created by vpatryshev on 6/11/17.
  */
abstract class SammonDimensionReducer
    [S <: VectorSpace, T <: VectorSpace]
    (val source: S,
     val target: T,
     val numSteps: Int,
     val errorPrecision: Double = 0.001) 
  extends DimensionReducer[S, T] {
  val DEBUG = false  
  type Target = T
  type MatrixLike = (Int, Int) => Double
  
  val dim: Int = source.dim

  protected val init: IndexedSeq[S#Vector] => IndexedSeq[target.Vector]

  def reduce(originalVectors: IndexedSeq[S#Vector]): IndexedSeq[T#Vector] = {
    val iterations = new Engine(originalVectors)
    iterations.run()
  }

  class Tangent(val vectors: IndexedSeq[target.Vector])
  
  case class State(vectors:IndexedSeq[target.MutableVector], var  matrix: MatrixLike) extends Mutable
  
  class Engine(originalVectors: IndexedSeq[S#Vector], magicFactor: Double = 0.01) {
    type DistanceMatrix = globalSpace.TriangularMatrix
    lazy val initialVectors = init(originalVectors)

    private lazy val minDouble = {
      val d = 0.7
      val s = originalVectors.length
      d/s/s
    }
    
    lazy val originalDistanceMatrix: MatrixLike = distanceMatrix(originalVectors)
    
    val size: Int = originalVectors.size
    val globalSpace = VectorSpace(size)

    private val evaluator = new Evaluator[State, Tangent] {
      override def cos(gradient1: Tangent, gradient2: Tangent): Double = {
        val moments = ((0.0, 0.0, 0.0) /: (gradient1.vectors zip gradient2.vectors)) {
          case ((prod, norm1, norm2), (v1, v2)) =>
            val n1 = Norm.l2(v1)
            val n2 = Norm.l2(v2)
            (prod + v1 * v2, norm1 + n1*n1, norm2 + n2*n2)
        }

        moments._1 / math.sqrt(moments._2 * moments._3)
      }
      
      override def targetFunction(position: State, maxValue: Double): Double = {
//        val erfTracker = new Tracker
        var s = 0.0
        for {
          i ← 0 until globalSpace.dim if s < maxValue * 2.5
          j ← 0 until i if s < maxValue * 2.5
        } {
          s += sammonErrorMeasure.op(originalDistanceMatrix(i, j), position.matrix(i, j))
        }
//        erfTracker << s"calculating error = $s"
        s
      }

      override def gradientAt(position: State): Tangent = {
        val vs = position.vectors
        val tangentVectors: IndexedSeq[target.Vector] = vs.indices map (p => `dE/dy`(vs, position.matrix, p))

        new Tangent(tangentVectors)
      }

      override def nudge(position: State, direction: Tangent, step: Double): Unit
      = {
        val vectors = position.vectors
        vectors zip direction.vectors foreach {
          case (a:target.MutableVector,b: target.Vector) => a.nudge(b, step)
        }
        position.matrix = distanceMatrix(vectors)
      }
    }

    def distanceMatrix[Space <: VectorSpace](vs: IndexedSeq[Space#Vector]): MatrixLike = {
      (i, j) => Norm.l2.distance(vs(i), vs(j))
    }
    

    lazy val numberOfVerticesInUnitCube: Int = 1 << target.dim

    private def stepInDirectionOf(i: Int): target.Vector = {
      val absNo = i % numberOfVerticesInUnitCube
      val d = minDouble
      val coordinates = (for {
        bitNo ← 0 until target.dim
      } yield if (((1 << bitNo) & absNo) == 0) d else -d) toArray
      
      new target.OnArray(coordinates)
    }

    def `dE/dy`(vectors: IndexedSeq[target.Vector], m: MatrixLike, p: Int): target.Vector = {
      val v: target.MutableVector = target.Zero.copy
      for {
        j ← 0 until size if j != p
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

    def run(): IndexedSeq[target.Vector] = {
      val t = new Tracker
      val finder = GradientDescentEngine[State, Tangent](evaluator, numSteps, errorPrecision, errorPrecision)
      val m0: MatrixLike = distanceMatrix(initialVectors)
      val state = State(initialVectors.map(_.copy), m0)
      val found1 = finder.find(state, 10.0)
      if (DEBUG) t << s"Found: $found1"
      state.vectors
    }
  }

}

object SammonDimensionReducer {
  def withPCA[S <: VectorSpace, T <: VectorSpace](source: S, target: T, numIterations: Int): DimensionReducer[S, T] = {
    val pca = pcaReducer(source, target, numIterations)
    val reducer: DimensionReducer[S, T] = new SammonDimensionReducer[S, T](source, target, numIterations) {
      protected val init: IndexedSeq[S#Vector] => IndexedSeq[target.Vector] =
        v => pca.reduce(v).map(_.asInstanceOf[target.Vector])
    }

    reducer
  }

  private def pcaReducer[S <: VectorSpace, T <: VectorSpace](source: S, target: T, numIterations: Int) =
    new PcaDimensionReducer[S, T](source, target, precision = 0.001, numIterations)
}