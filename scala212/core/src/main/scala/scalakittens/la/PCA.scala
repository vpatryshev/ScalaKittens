package scalakittens.la

/**
  * Created by vpatryshev on 5/17/17.
  */
object PCA {
  case class Iterations(space: VectorSpace)(precision: Double, maxRepeats: Int) {
    
    private def oneStep(m: space.SquareMatrix)(v: space.MutableVector): (space.MutableVector, Double) = {
      val vector: space.MutableVector = m * v
      val normalized: space.Vector = vector.normalize(Norm.l2)
      val v1: space.MutableVector = normalized.copy
      val d = Norm.l2(v1 - v)
      (v1, d)
    }
    
    def maxEigenValue(m: space.SquareMatrix): Option[(Double, space.MutableVector, Int)] = {
      require(m.nCols == m.nRows, s"Expected a square matrix, have ${m.nRows}⨯${m.nCols}")
      if (m.nCols == 0) None else
      if (m.nCols == 1) Some((m(0,0), space.Vector(1.0), 0)) else
      {
        val iterator = Iterator.iterate((space.unit(0).copy, Double.MaxValue, 0)) {
          case (v, d, i) =>
            val vectorToTuple = oneStep(m)(_)
            val (m1, d1) = vectorToTuple(v); (m1, d1, i + 1)
        }

        val goodData = iterator find (p => p._2 <= precision || p._3 > maxRepeats)

        val maybeTuple: Option[(Double, space.MutableVector, Int)] = goodData map {
          case (vector, delta, nSteps) => {
            val vector1: space.MutableVector = m * vector
            (vector1.sum / vector.sum, vector, nSteps)
          }
        }
        maybeTuple
      }
    }
    
    def oneEigenValueBasis(m: space.SquareMatrix): Option[(Double, space.UnitaryMatrix, Int)] = maxEigenValue(m) map {
      case (value: Double, vector: space.Vector, nIter) =>
        val vectors = space.buildOrthonormalBasis(vector)
        val basis: space.Basis = space.Basis.build(vectors)
        
        (value, basis.rotation, nIter)  
    }

    def buildEigenVectors(m: space.SquareMatrix, numberRequested: Int): Option[List[(Double, space.MutableVector)]] = {
      require (numberRequested <= m.nCols)
      space == m.codomain
      val v: space.Vector = space.Zero
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
