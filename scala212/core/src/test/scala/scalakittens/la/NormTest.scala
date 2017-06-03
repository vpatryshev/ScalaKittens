package scalakittens.la

import org.specs2.mutable.Specification

import Norm._

/**
  * Created by vpatryshev on 5/7/17.
  */
class NormTest extends Specification {
  import math._
  
  "Norm" should {

    "normalize" in {
      l2.normalize(Vector()) must_== Vector()
      l2.normalize(Vector(0)) must_== Vector(0)
      l2.normalize(Vector(0, 0, 0)) must_== Vector(0, 0, 0)
      l2.normalize(Vector(-sqrt(2), sqrt(2))) must_== Vector(-sqrt(0.5), sqrt(0.5))
    }

    "project" in {
      l2.project(Vector(1, 1), Vector(2, -2)) must_== Vector(0.0, 0.0)
        l2.project(Vector(1, 1), Vector(0, -2)) must_== Vector(-1.414213562373095, -1.414213562373095)
    }
    
    "build orthonormal basis 2x2" in {
      val vecs = l2.buildOrthonormalBasis(Vector(3, 4))
      for {i <- 0 until 2; j <- 0 until 2} {
        val prod = vecs(i)*vecs(j)
        val expected = if (i == j) 1.0 else 0.0
        abs(prod - expected) < 0.0001 aka s"@($i, $j): ${vecs(i)}*${vecs(j)} = $prod != $expected " must beTrue
      }

      val v0 = Vector(0.6000000000000001, 0.8)
      val v1 = Vector(-0.8000000000000001, 0.5999999999999999)
      vecs(0) must_== v0
      vecs(1) must_== v1
      vecs.toList must_== List(v0, v1)
      ok
    }

    "build orthonormal basis 3x3" in {
      val vecs = l2.buildOrthonormalBasis(Vector(3, 5, 4))
      for {i <- 0 until 3; j <- 0 until 3} {
        val prod = vecs(i)*vecs(j)
        val expected = if (i == j) 1.0 else 0.0
        abs(prod - expected) < 0.0001 aka s"@($i, $j): ${vecs(i)}*${vecs(j)} = $prod != $expected " must beTrue
      }
      ok
    }

    "build orthonormal basis 4x4" in {
      val vecs = l2.buildOrthonormalBasis(Vector(17, -1, 3, 4, 5))
      for {i <- 0 until 4; j <- 0 until 4} {
        val prod = vecs(i)*vecs(j)
        val expected = if (i == j) 1.0 else 0.0
        abs(prod - expected) < 0.0001 aka s"@($i, $j): ${vecs(i)}*${vecs(j)} = $prod != $expected " must beTrue
      }
      ok
    }
  }
}
