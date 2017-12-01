package scalakittens.la

import org.specs2.mutable.Specification
import Norm._

/**
  * Test suite for principal components analysis
  * Created by vpatryshev on 5/7/17.
  */
class PCATest extends Specification {
  import math._
  import Spaces._
  
  private def matrix(vs: VectorSpace, values: Array[Double]) = vs.squareMatrix(values)
  
  "Iterations method" should {
    "not fail on one simple case" in {
      val m = matrix(R2, Array(0.0, 100, 100, 0))
      val method = PCA.Iterations(0.001, 100)
      val Some((value: Double, vec: R2.Vector, nIter)) = method.maxEigenValue(R2)(m)

      value === 100.0

      val expected = R2.Vector(0.7071067811865476, 0.7071067811865476)
      vec === expected
    }

    "produce first eigenvector for a 3x3" in {
      val m = matrix(R3, Array(5.0, 1, 2, 1, 4, 1, 2, 1, 3))
      val method = PCA.Iterations(0.001, 1000)
      val Some((value: Double, vec: R3.Vector, nIter)) = method.maxEigenValue(R3)(m)
      l2(vec) must beCloseTo(1.0, 0.000001)
      val delta: Double = l2(m * vec / value - vec)
      delta < 0.0005 aka s"error=$delta" must beTrue

      value must beCloseTo(6.895, 0.001)
      val expected = R3.Vector(0.753,0.431,0.497)
      l2.distance(vec, expected) must be <= 0.002
    }
    
    "produce two eigenvectors for a 3x3 in many small steps" in {
      val n = 3
      val m = matrix(R3, Array(5.0, 1, 2, 1, 4, 1, 2, 1, 3))
      val method = PCA.Iterations(0.0001, 100)
      val Some((value1: Double, vector1: R3.Vector, nIter1)) = method.maxEigenValue(R3)(m)
      l2(vector1) must beCloseTo(1.0, 0.000001)
      val delta1: Double = l2(m * vector1 / value1 - vector1)
      delta1 < 0.0005 aka s"error1=$delta1" must beTrue

      value1 must beCloseTo(6.895, 0.001)
      val expected1 = R3.Vector(0.753,0.431,0.497)
      l2.distance(vector1, expected1) must be <= 0.001

      val newBasis = R3.unitaryMatrix(R3.buildOrthonormalBasis(vector1).map(_.copy))
      newBasis.column(0) === vector1
      val newBasisT = newBasis.transpose
      newBasisT.row(0) === vector1
      val checkBasis = R3.UnitMatrix rotate newBasis
      l2(checkBasis - R3.UnitMatrix) < 0.0001 aka checkBasis.toString must beTrue
      val m1 = m rotate newBasis.transpose

      for {i <- 1 until n
      } {
        abs(m1(i, 0)) < 0.0003 aka s"row $i: ${m1(i, 0)}" must beTrue
        abs(m1(0, i)) < 0.0003 aka s"col $i: ${m1(0, i)}" must beTrue
      }
      
      val submatrix = m1.projectToHyperplane().asInstanceOf[R2.SquareMatrix]
      
      val Some((value2: Double, vector2, _: Int)) = method.maxEigenValue(R2)(submatrix)

      l2(vector2) must beCloseTo(1.0, 0.000001)
      // TODO: get rid of this casting
      val v2Cast: R2.Vector = vector2.asInstanceOf[R2.Vector]
      val delta2: Double = l2(submatrix * v2Cast / value2 - v2Cast)
      delta2 < 0.0005 aka s"error2=$delta2" must beTrue

      value2 must beCloseTo(3.397, 0.001)
      val expected2 = R2.Vector(0.982,0.189)
      l2.distance(vector2, expected2) must be <= 0.001

      val vector2InHyperplane = R3.hyperplane.Vector(vector2)
      val vector3 = R3.injectFromHyperplane(vector2InHyperplane)
      val vector3InOurBasis = newBasis * vector3
      val vector3AfterM = m * vector3InOurBasis
      val diff = vector3AfterM - vector3InOurBasis * value2
      l2(diff) < 0.003 aka diff.toString must beTrue
    }

    "produce three eigenvectors for a 3x3 in steps" in {
      val m = matrix(R3, Array(5.0, 1, 2, 1, 4, 1, 2, 1, 3))
      val method = PCA.Iterations(0.0001, 100)
      val tuples = method.buildEigenVectors(R3)(m, 3)
      tuples mustNotEqual null

      val allThree = method.buildEigenVectors(R3)(m, 3)
      
//      allThree === 
//        (6.895151445258131,  R3.Vector(0.752603939328221,0.431649775140211,0.4972582650183391))::
//        (3.397409072501745,  R3.Vector(-0.4578505139226017,0.8857791252792523,-0.07594898366885619))::
//        (1.7075984315510695, R3.Vector(-0.4732443527486125,-0.17051044789375186,0.8642719304423923))::Nil
      
      for {
        (value, vector) <- allThree
      } l2(m * vector - vector * value) < 0.001 must beTrue
      
      val eigenBasis = R3.unitaryMatrix(allThree map (_._2.asInstanceOf[R3.MutableVector]))
      eigenBasis.isUnitary(0.001) must beTrue
    }
  }
}
