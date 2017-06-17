package scalakittens.la

import org.specs2.mutable.Specification

/**
  * Created by vpatryshev on 5/7/17.
  */
class VectorTest extends Specification {
  import Vector._
  import math._
  
  "Vector" should {
    "exist" in {
      ok
    }

    "know its length" in {
      val sut1 = Vector(new Array[Double](0))
      sut1.length must_== 0
      Vector(0).length must_== 0
      Vector(0.0).length must_== 1
      Vector(1.5, 2.25).length must_== 2
    }

    "have apply()" in {
      val sut = Vector(1.5, 2.25)
      sut(0) must_== 1.5
      sut(1) must_== 2.25
    }
    
    "have update()" in {
      val sut = Vector(0, 0).copy
      sut(0) = 1.5
      sut(0) must_== 1.5
      sut(1) must_== 0.0
      sut(1) = 2.25
      sut(0) must_== 1.5
      sut(1) must_== 2.25
    }

    "have foreach()" in {
      val sut = Vector(1.5, 2.25)
      var s = -3.75
      sut foreach (s += _)
      s must_== 0.0
    }

    "have forall()" in {
      Vector(0) forall (_ == Pi) must beTrue
      Vector(Pi, Pi, Pi) forall (_ == Pi) must beTrue
      Vector(Pi, 0, Pi) forall (_ == Pi) must beFalse
    }

    "have forall()" in {
      Vector(0) exists (_ == Pi) must beFalse
      Vector(Pi, Pi, Pi) exists (_ == Pi) must beTrue
      Vector(2.0, Pi, 1.0) exists (_ == Pi) must beTrue
      Vector(3.0, 2.0, 1.0) exists (_ == Pi) must beFalse
    }

    "have map()" in {
      (Vector(0) map ((x: Double) => "oi-vei") toList)   must_== Nil

      (Vector(3.0, 2.0, 1.0) map (":) " + _) toList) must_== List(":) 3.0", ":) 2.0", ":) 1.0")
    }

    "have /:" in {
      (":)" /: Vector(0))(_+_) must_== ":)"
      (-1.0 /: Vector(3.0, 2.0, 1.0))(_+_) must_== 5.0
    }

    "have copy" in {
      Vector(0).copy.length must_== 0
      val sut = Vector(Pi, E, 42.0).copy
      val copy = sut.copy
      sut *= 2.0
      copy must_== Vector(Pi, E, 42.0)
      sut *= 0.5
      copy *= 0.0
      sut must_== Vector(Pi, E, 42.0)
    }

    "multiply by scalar" in {
      val sut = Vector(1.5, 2.25) * 3.0
      sut must_== Vector(4.5, 6.75)
    }

    "divide by scalar" in {
      val sut = Vector(1.5, 2.25) / 2
      sut must_== Vector(0.75, 1.125)
    }

    "multiply by scalar in place" in {
      val sut = Vector(1.5, 2.25).copy
      sut *= 3.0
      sut must_== Vector(4.5, 6.75)
    }

    "divide by scalar in place" in {
      val sut = Vector(1.5, 2.25)
      sut / 2 must_== Vector(0.75, 1.125)
    }

    "multiply by another Vec" in {
      val sut = Vector(1.5, 2.25) * Vector(2.0, -3.0)
      sut must_== -3.75
    }

    "add another vec" in {
      val sut = Vector(3.25, -7.5) + Vector(6.0, 4.25)
      sut must_== Vector(9.25, -3.25)
    }

    "add another vec to mutable: it changes under our hands" in {
      val vec1 = Vector(3.25, -7.5).copy
      val sut1 = vec1 + Vector(6.0, 4.25)
      val sut2 = Vector(6.0, 4.25) + vec1
      vec1(0) = 0.0
      sut1 must_== Vector(6, -3.25)
      sut2 must_== Vector(6, -3.25)
    }

    "add another vec in place" in {
      val sut = Vector(3.25, -7.5).copy
      sut += Vector(6.0, 4.25)
      sut must_== Vector(9.25, -3.25)
    }

    "subtract another vec" in {
      val sut = Vector(3.25, -7.5) - Vector(6.0, 4.25)
      sut must_== Vector(-2.75, -11.75)
    }

    "subtract another vec where one is mutable: it changes under our hands" in {
      val vec1 = Vector(3.25, -7.5).copy
      val vec2 = Vector(6.0, 4.25).copy
      vec1(0) = 0
      vec2(0) = 0
      val sut = vec1 - vec2
      sut must_== Vector(0, -11.75)
    }

    "subtract another vec in place" in {
      val sut = Vector(3.25, -7.5).copy
      sut -= Vector(6.0, 4.25)
      sut must_== Vector(-2.75, -11.75)
    }

    "get nudged by another vec" in {
      val sut = Vector(3.25, -7.5).copy
      sut.nudge(Vector(6.0, 4.75), 2.0)
      sut must_== Vector(15.25, 2.0)
    }

    "have ::" in {
      val v0 = Vector()
      val sut = Pi::E::42::v0
      sut must_== Vector(Pi, E, 42)
      ok
    }
  }

  "factory" should {
    "be able to produce empty vector" in {
      Zero(0) must_== Vector(0)
    }
    
    "build zero vector" in {
      Zero(3) must_== Vector(0.0, 0.0, 0.0)
    }

    "build 'random' vector" in {
      val sut = RandomCube(3, 314159)()
      sut forall (abs(_) <= 1.0)
    }
  }
  
  "Vector object" should {
    "produce a vector from function" in {
      val sut1 = new Vector.OnFunction(0, i => 42)
      sut1 must_== Vector()
      new Vector.OnFunction(6, i => (i*sqrt(i)).toInt) must_== Vector(0, 1, 2, 5, 8, 11)
    }
    
    "produce a unit vector" in {
      Vector.unit(1, 0) must_== Vector(1.0)
      for (i <- 0 until 10) {
        val sut = Vector.unit(10, i)
        for (j <- 0 until 10) {
          sut(j) must_== (if (i == j) 1.0 else 0.0)
        }
      }
      ok
    }
    
    "not fail with subspaces" in {
      val space0 = VectorSpace(3)
      def recurse(s: VectorSpace)(v: s.Vector): Unit = {
        if (s.dim == 0) v else {
          val v1 = s.projectToHyperplane(v) * 2
          recurse(s.hyperplane)(v1)
        }
      }
      val sut = space0.const(3)
      recurse(space0)(sut) must_== space0.Vector(3, 6, 9)
      ok
    }
  }
}
