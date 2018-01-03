package scalakittens.la

import language.existentials
import Norm.l2
/**
  * Created by vpatryshev on 5/17/17.
  */
object PCA {
  case class Iterations(precision: Double, maxRepeats: Int) {

    private def oneStep(space: VectorSpace)(m: space.SquareMatrix, v: space.Vector): (space.Vector, Double) = {
      val vector = m * v
      val normalized: space.Vector = vector.normalize(l2)
      val v1 = (normalized + v) / 2
      val d = l2.distance(v1, v)
      (v1, d)
    }
    
    def maxEigenValue(space: VectorSpace)(m: space.SquareMatrix): Option[(Double, space.Vector, Int)] = {

      val unitVector = m.domain.unit(0)
      if (m.nCols == 0) None else if (m.nCols == 1) Some((m(0, 0), unitVector, 0)) else {
        val iterator = Iterator.iterate((unitVector, Double.MaxValue, 0)) {
          case (v, d, i) =>
            val (v1, d1) = oneStep(space)(m, v)
            (v1, d1, i + 1)
        }

        val goodData = iterator find (p => p._2 <= precision / 2 || p._3 > maxRepeats)

        val maybeTuple: Option[(Double, space.Vector, Int)] = goodData map {
          case (vector, delta, nSteps) =>
            val vector1: space.Vector = m * vector
            (vector1.sum / vector.sum, vector, nSteps)
        }
        maybeTuple
      }
    }

    def buildEigenVectors(space: VectorSpace)(m: space.SquareMatrix, numberRequested: Int): List[(Double, space.Vector)] = {
      eigenVectorFinder(space).runOn(m, numberRequested)
    }

    // need to specify space type because we go deeper into subspace
    def eigenVectorFinder(s: VectorSpace): EigenVectorFinder[s.type] = new EigenVectorFinder(s)

    class EigenVectorFinder[S <: VectorSpace](val s: S) {

      def oneEigenValueBasis(m: s.SquareMatrix): Option[(Double, s.UnitaryMatrix, Int)] = maxEigenValue(s)(m) map {
        case (value: Double, vector: s.Vector, nIter) =>
          val vectors = s.buildOrthonormalBasis(vector)
          val basis: s.Basis = s.Basis(vectors)

          (value, basis.rotation, nIter)
      }

      def runOn(m: s.SquareMatrix, numberRequested: Int): List[(Double, s.Vector)] = {
        require(numberRequested <= s.dim)
        if (numberRequested == 0) Nil else {

          oneEigenValueBasis(m) match {
            case Some((eigenValue, basis, _)) =>
              val submatrix: s.hyperplane.SquareMatrix = m.projectToHyperplane(basis)
              val tail = eigenVectorFinder(s.hyperplane).runOn(submatrix, numberRequested - 1)
              val newTail: List[(Double, s.Vector)] = tail map { case (value, vector) =>
                val vector1: s.Vector = s.injectFromHyperplane(vector)
                (value, basis * vector1)
              }
              val v: s.Vector = basis.column(0)
              ((eigenValue, v) :: newTail) map { case (value, vector) => (value, vector) }
            case None => Nil
          }
        }
      }
    }
  }
}
