package scalakittens.la

import org.specs2.mutable.Specification

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
    } m.set(i, j, values(i*w+j))
    
    m
  }
  
  "Iterations method" should {
    "produce first eigenvector for a 3x3" in {
      val m = matrix(3, 3, 5, 1, 2, 1, 4, 1, 2, 1, 3)
      val method = PCA.Iterations(0.001, 100)
      val Some((value: Double, vector: Vector, nIter)) = method.eigenValue(m)
      
      value must_== 6.895482499314163
      
      vector must_== Vector(0.7528109532832238, 0.431249716162522, 0.4972920177587291)
      vector.l2 must_== 1.0
      val delta: Double = (m * vector / value - vector).l2
      delta < 0.001 aka s"error=$delta" must beTrue
    }
    
    "produce two eigenvectors for a 3x3" in {
      val m = matrix(3, 3, 5, 1, 2, 1, 4, 1, 2, 1, 3)
      val method = PCA.Iterations(0.001, 100)
      val Some((value: Double, vector: Vector, nIter)) = method.eigenValue(m)

      value must_== 6.895482499314163
      vector must_== Vector(0.7528109532832238, 0.431249716162522, 0.4972920177587291)
      vector.l2 must_== 1.0
      
      nIter < 20 must beTrue
    }
  }
}
