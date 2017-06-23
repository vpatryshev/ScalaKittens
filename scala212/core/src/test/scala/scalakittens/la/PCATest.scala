package scalakittens.la

import org.specs2.mutable.Specification
import Norm._

/**
  * Created by vpatryshev on 5/7/17.
  */
class PCATest extends Specification {
  import math._
  import Spaces._
  
  def matrix[VS <: VectorSpace](vs: VS) = (values: Array[Double]) => vs.squareMatrix(values.toArray)

  //    vs.squareMatrix((i, j) => values(i*vs.dim+j))
  
  "Iterations method" should {
    "not fail when (1,0) is orthogonal to the main eigenvector" in {
      val m = matrix(R2)(Array(0.0, 100, 100, 0))
      val method = PCA.Iterations(0.001, 100)
      val Some((value: Double, vec: R2.Vector, nIter)) = method.maxEigenValue(R2)(m)

      value === 100.0

      vec === R2.Vector(0.0,1.0)
    }

    "produce first eigenvector for a 3x3" in {
      val m = matrix(R3)(Array(5.0, 1, 2, 1, 4, 1, 2, 1, 3))
      val method = PCA.Iterations(0.001, 100)
      val Some((value: Double, vec: R3.Vector, nIter)) = method.maxEigenValue(R3)(m)
      
      value === 6.895482499314163
      
      vec === R3.Vector(0.7528109532832238,0.431249716162522,0.4972920177587291)
      l2(vec) === 1.0
      val delta: Double = l2(m * vec / value - vec)
      delta < 0.0005 aka s"error=$delta" must beTrue
    }
    
    "produce two eigenvectors for a 3x3 in many small steps" in {
      val n = 3
      val m = matrix(R3)(Array(5.0, 1, 2, 1, 4, 1, 2, 1, 3))
      val method = PCA.Iterations(0.0001, 100)
      val Some((value1: Double, vector1: R3.Vector, nIter1)) = method.maxEigenValue(R3)(m)

      value1 === 6.8951514452581313
      vector1 === R3.Vector(0.752603939328221, 0.431649775140211, 0.4972582650183391)
      l2(vector1) === 1.0
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
      
      val submatrix:R2.SquareMatrix = m1.projectToHyperplane().asInstanceOf[R2.SquareMatrix]
      val Some((value2: Double, vector2: R2.Vector, _: Int)) = method.maxEigenValue(R2)(submatrix)
      value2 === 3.397409072501745
      l2(vector2) === 1.0

      val expected2 = R2.Vector(0.9819718282130517, 0.18902732235292571)
      vector2.toString === expected2.toString
      val d = vector2 - expected2
      l2(d) must_== 0.0
      val same2 = vector2 == expected2
      same2 must beTrue
      vector2 === expected2

      val vector3 = R3.injectFromHyperplane[R2.type](vector2)
      val vector3InOurBasis = newBasis * vector3
      val vector3AfterM = m * vector3InOurBasis
      val diff = vector3AfterM - vector3InOurBasis * value2
      l2(diff) < 0.0003 aka diff.toString must beTrue
    }

    "produce three eigenvectors for a 3x3 in steps" in {
      val m = matrix(R3)(Array(5.0, 1, 2, 1, 4, 1, 2, 1, 3))
      val method = PCA.Iterations(0.0001, 100)
      val tuples = method.buildEigenVectors(R3)(m, 3)
      tuples mustNotEqual null
      val finder = new method.EigenVectorFinder[R3.type](R3)

      val Some((eigenValue1: Double, basis: R3.UnitaryMatrix, nIter1)) = finder.oneEigenValueBasis(m)

      eigenValue1 === 6.8951514452581313
      basis.column(0) === R3.Vector(0.752603939328221, 0.431649775140211, 0.4972582650183391)

      val allThree = method.buildEigenVectors(R3)(m, 3)
      
      allThree === 
        (6.895151445258131,  R3.Vector(0.752603939328221,0.431649775140211,0.4972582650183391))::
        (3.397409072501745,  R3.Vector(-0.4578505139226017,0.8857791252792523,-0.07594898366885619))::
        (1.7075984315510695, R3.Vector(-0.4732443527486125,-0.17051044789375186,0.8642719304423923))::Nil
      
      
      for {
        (value, vector) <- allThree
      } l2(m * vector - vector * value) < 0.001 must beTrue
      
      val eigenBasis = R3.unitaryMatrix(allThree map (_._2.asInstanceOf[R3.MutableVector]))
      eigenBasis.isUnitary(0.001) must beTrue
    }
  }
}
