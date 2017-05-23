package scalakittens.la

import org.specs2.mutable.Specification
import Norm._

/**
  * Created by vpatryshev on 5/7/17.
  */
class PCATest extends Specification {
  import math._
  
  def matrix(h: Int, w: Int, values: Double*): Matrix = {
    val m = Matrix(h, w)
    for {
      i <- 0 until h
      j <- 0 until w
    } m(i, j) = values(i*w+j)
    
    m
  }
  
  "Iterations method" should {
    "produce first eigenvector for a 3x3" in {
      val m = matrix(3, 3, 5, 1, 2, 1, 4, 1, 2, 1, 3)
      val method = PCA.Iterations(0.001, 100)
      val Some((value: Double, vec: Vector, nIter)) = method.eigenValue(m)
      
      value must_== 6.895482499314164
      
      vec must_== Vector(0.7528109532832238, 0.431249716162522, 0.49729201775872905)
      l2(vec) must_== 1.0
      val delta: Double = l2(m * vec / value - vec)
      delta < 0.0005 aka s"error=$delta" must beTrue
    }
    
    "produce two eigenvectors for a 3x3" in {
      val n = 3
      val m = matrix(3, 3, 5, 1, 2, 1, 4, 1, 2, 1, 3)
      val method = PCA.Iterations(0.0001, 100)
      val Some((value1: Double, vector1: Vector, nIter1)) = method.eigenValue(m)

      value1 must_== 6.8951514452581313
      vector1 must_== Vector(0.752603939328221, 0.431649775140211, 0.4972582650183391)
      l2(vector1) must_== 1.0
      val newBasis = Matrix.Unitary(vector1.buildOrthonormalBasis)
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
      val Some((value2: Double, vector2: Vector, nIter2: Int)) = method.eigenValue(submatrix)
      value2 must_== 3.397409072501745
      l2(vector2) must_== 1.0
      vector2 must_== Vector(0.9819718282130517, 0.18902732235292566)

      val vector3 = 0.0 :: vector2
      val vector3InOurBasis = newBasis * vector3
      val vector3AfterM = m * vector3InOurBasis
      val diff = vector3AfterM - vector3InOurBasis * value2
      l2(diff) < 0.0003 aka diff.toString must beTrue
      
      ok
    }
  }
}
