package scalakittens.la

import org.specs2.mutable.Specification
import Norm._

/**
  * Created by vpatryshev on 5/7/17.
  */
class PCATest extends Specification {
  import math._
  import Spaces._
  
  def matrix[VS <: VectorSpace](vs: VS, values: Double*) = 
    vs.squareMatrix(values.toArray)

  //    vs.squareMatrix((i, j) => values(i*vs.dim+j))
  
  "Iterations method" should {
    "not fail when (1,0) is orthogonal to the main eigenvector" in {
      val m = matrix(R2, 0, 100, 100, 0)
      val method = PCA.Iterations(R2)(0.001, 100)
      val Some((value: Double, vec: R3.Vector, nIter)) = method.maxEigenValue(R2)(m)

      value must_== 100.0

      vec must_== Vector(0.0,1.0)
    }

    "produce first eigenvector for a 3x3" in {
      val m = matrix(R3, 5, 1, 2, 1, 4, 1, 2, 1, 3)
      val method = PCA.Iterations(R3)(0.001, 100)
      val Some((value: Double, vec: R3.Vector, nIter)) = method.maxEigenValue(R3)(m)
      
      value must_== 6.895482499314163
      
      vec must_== Vector(0.7528109532832238,0.431249716162522,0.4972920177587291)
      l2(vec) must_== 1.0
      val delta: Double = l2(m * vec / value - vec)
      delta < 0.0005 aka s"error=$delta" must beTrue
    }
    
    "produce two eigenvectors for a 3x3 in many small steps" in {
      val n = 3
      val m = matrix(R3, 5, 1, 2, 1, 4, 1, 2, 1, 3)
      val method = PCA.Iterations(R3)(0.0001, 100)
      val Some((value1: Double, vector1: R3.Vector, nIter1)) = method.maxEigenValue(R3)(m)

      value1 must_== 6.8951514452581313
      vector1 must_== R3.Vector(0.752603939328221, 0.431649775140211, 0.4972582650183391)
      l2(vector1) must_== 1.0
      val newBasis = R3.UnitaryMatrix(R3.buildOrthonormalBasis(vector1))
      newBasis.column(0) must_== vector1
      val newBasisT = newBasis.transpose
      newBasisT.row(0) must_== vector1
      val checkBasis = Matrix.Unit(n) rotate newBasis
      l2(checkBasis - Matrix.Unit(n)) < 0.0001 aka checkBasis.toString must beTrue
      val m1 = m rotate newBasis.transpose

      for {i <- 1 until n
      } {
        abs(m1(i, 0)) < 0.0003 aka s"row $i: ${m1(i, 0)}" must beTrue
        abs(m1(0, i)) < 0.0003 aka s"col $i: ${m1(0, i)}" must beTrue
      }
      
      val submatrix = m1.dropColumn(0).dropRow(0)
      val Some((value2: Double, vector2: Vector, _: Int)) = method.maxEigenValue(submatrix)
      value2 must_== 3.397409072501745
      l2(vector2) must_== 1.0
      vector2 must_== Vector(0.9819718282130517,0.18902732235292571)

      val vector3 = 0.0 :: vector2
      val vector3InOurBasis = newBasis * vector3
      val vector3AfterM = m * vector3InOurBasis
      val diff = vector3AfterM - vector3InOurBasis * value2
      l2(diff) < 0.0003 aka diff.toString must beTrue
    }

    "produce three eigenvectors for a 3x3 in steps" in {
      val m = matrix(R3, R3, 5, 1, 2, 1, 4, 1, 2, 1, 3)
      val method = PCA.Iterations(R3)(0.0001, 100)
      val Some((eigenValue1: Double, basis: R3.UnitaryMatrix, nIter1)) = method.oneEigenValueBasis(R3)(m)

      eigenValue1 must_== 6.8951514452581313
      basis.column(0) must_== Vector(0.752603939328221, 0.431649775140211, 0.4972582650183391)

      val Some(allThree) = method.buildEigenVectors(R3)(m, 3)
      
      allThree must_== 
        (6.895151445258131,  Vector(0.752603939328221,0.431649775140211,0.4972582650183391))::
        (3.397409072501745,  Vector(-0.4578505139226017,0.8857791252792523,-0.07594898366885619))::
        (1.7075984315510695, Vector(-0.4732443527486125,-0.17051044789375186,0.8642719304423923))::Nil
      
      
      for {
        (value, vector) <- allThree
      } l2(m * vector - vector * value) < 0.001 must beTrue
      
      val eigenBasis = Matrix.Unitary(allThree map (_._2))
      eigenBasis.isUnitary(0.001) must beTrue
    }
  }
}
