package scalakittens.la

import org.specs2.mutable.Specification

/**
  * Created by vpatryshev on 5/7/17.
  */
class VectorTest extends Specification {
  import math._
  import Spaces._

  "VectorSpace" should {

    "build unit cube" in {
      val vectors = R3.Vector(0,1,2)::R3.Vector(0,2,4)::R3.Vector(-2,3,10)::Nil
      val unitCube = R3.unitCube(vectors)

      unitCube(R3.Vector(0,0,0)) aka unitCube.toString must_== R3.Vector(1,-0.5,-0.25)
      unitCube(R3.Vector(-1, 0, 0)) must_== R3.Vector(0.5,-0.5,-0.25)
      unitCube(R3.Vector(0,1,0)) must_== R3.Vector(1,0,-0.25)
      unitCube(R3.Vector(0,0,1)) must_== R3.Vector(1.0,-0.5,-0.125)
    }
    "build orthonormal basis 2x2" in {
      val vecs = R2.buildOrthonormalBasis(R2.Vector(3, 4))
      for {i <- 0 until 2; j <- 0 until 2} {
        val prod = vecs(i)*vecs(j)
        val expected = if (i == j) 1.0 else 0.0
        abs(prod - expected) < 0.0001 aka s"@($i, $j): ${vecs(i)}*${vecs(j)} = $prod != $expected " must beTrue
      }

      val v0 = R2.Vector(0.6000000000000001, 0.8)
      val v1 = R2.Vector(-0.8000000000000001, 0.5999999999999999)
      vecs(0) must_== v0
      vecs(1) must_== v1
      vecs.toList must_== List(v0, v1)
      ok
    }

    "build orthonormal basis 3x3" in {
      val vecs = R3.buildOrthonormalBasis(R3.Vector(3, 5, 4))
      for {i <- 0 until 3; j <- 0 until 3} {
        val prod = vecs(i)*vecs(j)
        val expected = if (i == j) 1.0 else 0.0
        abs(prod - expected) < 0.0001 aka s"@($i, $j): ${vecs(i)}*${vecs(j)} = $prod != $expected " must beTrue
      }
      ok
    }

    "build orthonormal basis 4x4" in {
      val vecs = R4.buildOrthonormalBasis(R4.Vector(17, -1, 3, 4))
      for {i <- 0 until 4; j <- 0 until 4} {
        val prod = vecs(i)*vecs(j)
        val expected = if (i == j) 1.0 else 0.0
        abs(prod - expected) < 0.0001 aka s"@($i, $j): ${vecs(i)}*${vecs(j)} = $prod != $expected " must beTrue
      }
      ok
    }

  }
  
  "Vector" should {
    "exist" in {
      ok
    }
    
    "know its length" in {
      val sut1 = R0.Vector(new Array[Double](0))
      sut1.length === 0
      R1.Vector(0.0).length === 1
      R2.Vector(1.5, 2.25).length === 2
    }
    
    "fail on wrong length" in {
      R0.Vector(42.0) must throwA[Exception]
      R1.Vector() must throwA[Exception]
      R1.Vector(2.0, 42.0) must throwA[Exception]
      R2.Vector(6.5) must throwA[Exception]
    }

    "have apply()" in {
      val sut = R2.Vector(1.5, 2.25)
      sut(0) === 1.5
      sut(1) === 2.25
    }
    
    "have update()" in {
      val sut =R2.Vector(0, 0).copy
      sut(0) = 1.5
      sut(0) === 1.5
      sut(1) === 0.0
      sut(1) = 2.25
      sut(0) === 1.5
      sut(1) === 2.25
    }

    "have foreach()" in {
      val sut = R2.Vector(1.5, 2.25)
      var s = -3.75
      sut foreach (s += _)
      s === 0.0
    }

    "have forall()" in {
      R0.Vector() forall (_ == Pi) must beTrue
      R3.Vector(Pi, Pi, Pi) forall (_ == Pi) must beTrue
      R3.Vector(Pi, 0, Pi) forall (_ == Pi) must beFalse
    }

    "have exists()" in {
      R0.Vector() exists (Pi == _*1.1) must beFalse
      R3.Vector(Pi, Pi, Pi) exists (x => x*x == Pi*Pi) must beTrue
      R3.Vector(2.0, Pi, 1.0) exists (x => x*x == Pi*Pi) must beTrue
      R3.Vector(2.0, 2.0, 1.0) exists (x => x*x == Pi*Pi) must beFalse
    }

    "have map()" in {
      (R0.Vector() map ((x: Double) => "oi-vei") toList)   === Nil

      (R3.Vector(3.0, 2.0, 1.0) map (":) " + _) toList) === List(":) 3.0", ":) 2.0", ":) 1.0")
    }

    "have /:" in {
      (":)" /: R0.Vector())(_+_) === ":)"
      (":)" /: R1.Vector(0))(_+_) === ":)0.0" // this sucks, but it's life... or is it?
      (-1.0 /: R3.Vector(3.0, 2.0, 1.0))(_+_) === 5.0
    }

    "have copy" in {
      R0.Vector().copy.length === 0
      val sut = R3.Vector(Pi, E, 42.0).copy
      val copy = sut.copy
      sut *= 2.0
      copy === R3.Vector(Pi, E, 42.0)
      sut *= 0.5
      copy *= 0.0
      sut === R3.Vector(Pi, E, 42.0)
    }

    "multiply by scalar" in {
      val sut = R2.Vector(1.5, 2.25) * 3.0
      sut === R2.Vector(4.5, 6.75)
    }

    "divide by scalar" in {
      val sut = R2.Vector(1.5, 2.25) / 2
      sut === R2.Vector(0.75, 1.125)
    }

    "multiply by scalar in place" in {
      val sut = R2.Vector(1.5, 2.25).copy
      sut *= 3.0
      sut === R2.Vector(4.5, 6.75)
    }

    "divide by scalar in place" in {
      val sut = R2.Vector(1.5, 2.25)
      sut / 2 === R2.Vector(0.75, 1.125)
    }

    "multiply by another Vec" in {
      val sut = R2.Vector(1.5, 2.25) * R2.Vector(2.0, -3.0)
      sut === -3.75
    }

    "add another vec" in {
      val sut = R2.Vector(3.25, -7.5) + R2.Vector(6.0, 4.25)
      sut === R2.Vector(9.25, -3.25)
    }

    "add another vec to mutable: it changes under our hands" in {
      val vec1 = R2.Vector(3.25, -7.5).copy
      val sut1 = vec1 + R2.Vector(6.0, 4.25)
      val sut2 = R2.Vector(6.0, 4.25) + vec1
      vec1(0) = 0.0
      sut1 === R2.Vector(6, -3.25)
      sut2 === R2.Vector(6, -3.25)
    }

    "add another vec in place" in {
      val sut = R2.Vector(3.25, -7.5).copy
      sut += R2.Vector(6.0, 4.25)
      sut === R2.Vector(9.25, -3.25)
    }

    "subtract another vec" in {
      val sut = R2.Vector(3.25, -7.5) - R2.Vector(6.0, 4.25)
      sut === R2.Vector(-2.75, -11.75)
    }

    "subtract another vec where one is mutable: it changes under our hands" in {
      val vec1 = R2.Vector(3.25, -7.5).copy
      val vec2 = R2.Vector(6.0, 4.25).copy
      vec1(0) = 0
      vec2(0) = 0
      val sut = vec1 - vec2
      sut === R2.Vector(0, -11.75)
    }

    "subtract another vec in place" in {
      val sut = R2.Vector(3.25, -7.5).copy
      sut -= R2.Vector(6.0, 4.25)
      sut === R2.Vector(-2.75, -11.75)
    }

    "get nudged by another vec" in {
      val sut = R2.Vector(3.25, -7.5).copy
      sut.nudge(R2.Vector(6.0, 4.75), 2.0)
      sut === R2.Vector(15.25, 2.0)
    }

    "project" in {
      R2.project(R2.Vector(1, 1), R2.Vector(2, -2)) must_== R2.Vector(0.0, 0.0)
      R2.project(R2.Vector(1, 1), R2.Vector(0, -2)) must_== R2.Vector(-1.414213562373095, -1.414213562373095)
    }
  }

  "factory" should {
    "be able to produce empty vector" in {
      R0.Zero === R0.Vector()
    }
    
    "build zero vector" in {
      R3.Zero === R3.Vector(0.0, 0.0, 0.0)
    }

    "build 'random' vector" in {
      val sut = R3.RandomCube(314159)()
      sut forall (abs(_) <= 1.0)
    }
  }
  
  "Vector object" should {
    "produce a vector from function" in {
      val sut1 = new R0.OnFunction(i => 42)
      sut1 === R0.Vector()
      new Spaces.R6.OnFunction(i => (i*sqrt(i)).toInt) === R6.Vector(0, 1, 2, 5, 8, 11)
    }
    
    "produce a unit vector" in {
      R1.unit(0) === R1.Vector(1.0)
      for (i <- 0 until 10) {
        val sut = R10.unit(i)
        for (j <- 0 until 10) {
          sut(j) === (if (i == j) 1.0 else 0.0)
        }
      }
      ok
    }
    
    "compile with subspaces" in {
      val space0 = VectorSpace(3)
      def recurse(s: VectorSpace)(v: s.Vector): Unit = {
        val ignoreme = if (s.dim == 0) v else {
          val v1 = s.projectToHyperplane(v) * 2
          recurse(s.hyperplane)(v1)
        }
        ignoreme mustNotEqual null
        ()
      }
      val sut = space0.const(3)
      recurse(space0)(sut)
      ok
    }
  }
}
