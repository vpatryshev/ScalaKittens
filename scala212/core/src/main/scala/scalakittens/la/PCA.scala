package scalakittens.la

import javafx.scene.transform
import javafx.scene.transform.MatrixType

/**
  * Created by vpatryshev on 5/17/17.
  */
object PCA {
  case class Iterations(space: VectorSpace)(precision: Double, maxRepeats: Int) {

    private def oneStep(space: VectorSpace)(m: space.SquareMatrix)(v: space.MutableVector): (space.MutableVector, Double) = {
      val vector: space.MutableVector = m * v
      val normalized: space.Vector = vector.normalize(Norm.l2)
      val v1: space.MutableVector = normalized.copy
      val d = Norm.l2(v1 - v)
      (v1, d)
    }

    def maxEigenValue(space: VectorSpace)(m: space.SquareMatrix): Option[(Double, space.MutableVector, Int)] = {
      require(m.domain == m.codomain, s"Expected a square matrix, have ${m.nRows}⨯${m.nCols}")
      if (m.nCols == 0) None else if (m.nCols == 1) Some((m(0, 0), space.Vector(1.0), 0)) else {
        val iterator = Iterator.iterate((space.unit(0).copy, Double.MaxValue, 0)) {
          case (v, d, i) =>
            val vectorToTuple = oneStep(space)(m)(_: space.MutableVector)
            val (m1, d1) = vectorToTuple(v)
            (m1, d1, i + 1)
        }

        val goodData = iterator find (p => p._2 <= precision || p._3 > maxRepeats)

        val maybeTuple: Option[(Double, space.MutableVector, Int)] = goodData map {
          case (vector, delta, nSteps) => 
            val vector1: space.MutableVector = m * vector
            (vector1.sum / vector.sum, vector, nSteps)
        }
        maybeTuple
      }
    }

    def buildEigenVectors(m: space.SquareMatrix, numberRequested: Int): List[(Double, space.Vector)] = {
      val finder = new EigenVectorFinder[space.type](space)
      finder.runOn(m, numberRequested)
    }

    class EigenVectorFinder[S <: VectorSpace](val s: S) {

      def oneEigenValueBasis(m: s.SquareMatrix): Option[(Double, s.UnitaryMatrix, Int)] = maxEigenValue(s)(m) map {
        case (value: Double, vector: s.Vector, nIter) =>
          val vectors = s.buildOrthonormalBasis(vector)
          val basis: s.Basis = s.Basis.build(vectors)

          (value, basis.rotation, nIter)
      }


      def runOn(m: s.SquareMatrix, numberRequested: Int): List[(Double, S#Vector)] = {
        require(numberRequested <= s.dim)
        if (numberRequested == 0) Nil else {
          val finder: EigenVectorFinder[s.hyperplane.type] = new EigenVectorFinder[s.hyperplane.type](s.hyperplane)

          oneEigenValueBasis(m) match {
            case Some((eigenValue, basis, _)) =>
              val submatrix: s.hyperplane.SquareMatrix = m.projectToHyperplane(basis.asInstanceOf[s.UnitaryMatrix])
              val tail = finder.runOn(submatrix, numberRequested - 1)
              val newTail = tail map { case (value, vector) => 
                val vector1: s.Vector = s.injectFromHyperplane(vector)
                (value, basis * vector1) 
              }
              ((eigenValue, basis.column(0)) :: newTail) map { case (value, vector) => (value, vector) }
            case None => Nil
          }
        }
      }
    }

  }
}
