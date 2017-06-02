package scalakittens.la

import language.postfixOps

/**
  * Created by vpatryshev on 5/17/17.
  */
object PCA {
  case class Iterations(precision: Double, maxRepeats: Int) {
    
    private def oneStep(m: Matrix, v: Vector): (Vector, Double) = {
      val v1 = Norm.l2.normalize(m * v)
      val d = Norm.l2(v1 - v)
      (v1, d)
    }
    
    def maxEigenValue(m: Matrix): Option[(Double, Vector, Int)] = {
      require(m.nCols == m.nRows, s"Expected a square matrix, have ${m.nRows}⨯${m.nCols}")
      if (m.nCols == 0) None else
      if (m.nCols == 1) Some((m(0,0), Vector(1.0), 0)) else
      {
        val iterator = Iterator.iterate((Vector.unit(m.nCols, 0), Double.MaxValue, 0)) {
          case (v, d, i) => val (m1, d1) = oneStep(m, v); (m1, d1, i + 1)
        }

        val goodData = iterator find (p => p._2 <= precision || p._3 > maxRepeats)

        goodData map { 
          case (vector, delta, nSteps) => ((m * vector).sum / vector.sum, vector, nSteps)
        }
      }
    }
    
    def oneEigenValueBasis(m: Matrix): Option[(Double, UnitaryMatrix, Int)] = maxEigenValue(m) map {
      case (value: Double, vector: Vector, nIter) =>
        (value, Matrix.Unitary(Norm.l2.buildOrthonormalBasis(vector)), nIter)  
    }

    def buildEigenVectors(m: Matrix, numberRequested: Int): Option[List[(Double, Vector)]] = {
      require (numberRequested <= m.nCols)
      
      if (numberRequested == 0) Some(Nil) else {
        for {
          (eigenValue, basis, _) <- oneEigenValueBasis(m)
          submatrix              =  m.projectToHyperplane(basis)
          tail                   <- buildEigenVectors(submatrix, numberRequested - 1)
        } yield {
          val newTail = tail map { case (value, vector) => (value, basis*(0::vector)) }
          (eigenValue, basis.column(0)) :: newTail
        }
      }
    }
  }
}
