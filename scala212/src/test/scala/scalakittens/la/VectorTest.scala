package scalakittens.la

import org.specs2.mutable.Specification

/**
  * Created by vpatryshev on 5/7/17.
  */
class VectorTest extends Specification {
  import Vector._
  import math._
  
  "Vector" should {
    "know its length" in {
      Vector(new Array[Double](0)).length must_== 0
      Vector(0).length must_== 0
      Vector(0.0).length must_== 1
      Vector(1.5, 2.25).length must_== 2
    }
    
    "have apply()" in {
      val sut = Vector(1.5, 2.25)
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
      (Vector(0) map (_ => "oi-vei") toList) must_== Nil
      (Vector(3.0, 2.0, 1.0) map (":) " + ) toList) must_== List(":) 3.0", ":) 2.0", ":) 1.0")
    }

    "have /:" in {
      (":)" /: Vector(0))(_+_) must_== ":)"
      (-1.0 /: Vector(3.0, 2.0, 1.0))(_+_) must_== 5.0
    }
    
    "have copy" in {
      Vector(0).copy.length must_== 0
      val sut = Vector(Pi, E, 42.0)
      val copy = sut.copy
      sut *= 2.0
      copy must_== Vector(Pi, E, 42.0)
      sut *= 0.5
      copy *= 0.0
      sut must_== Vector(Pi, E, 42.0)
    }
    
    "multiply by scalar" in {
      val sut = Array(1.5, 2.25) * 3.0
      sut.data must_== Array(4.5, 6.75)
    }

    "divide by scalar" in {
      val sut = Array(1.5, 2.25) / 2
      sut.data must_== Array(0.75, 1.125)
    }

    "multiply by scalar in place" in {
      val sut = Array(1.5, 2.25)
      sut *= 3.0
      sut.data must_== Array(4.5, 6.75)
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

    "add another vec in place" in {
      val sut = Vector(3.25, -7.5)
      sut += Vector(6.0, 4.25)
      sut must_== Vector(9.25, -3.25)
    }

    "subtract another vec" in {
      val sut = Vector(3.25, -7.5) - Vector(6.0, 4.25)
      sut must_== Vector(-2.75, -11.75)
    }

    "subtract another vec in place" in {
      val sut = Vector(3.25, -7.5)
      sut -= Vector(6.0, 4.25)
      sut must_== Vector(-2.75, -11.75)
    }

    "get nudged by another vec" in {
      val sut = Vector(3.25, -7.5)
      sut.nudge(Vector(6.0, 4.75), 2.0)
      sut must_== Vector(15.25, 2.0)
    }
  }

  "factory" should {
    "be able to produce empty vector" in {
      Zero(0)() must_== Vector(0)
    }
    
    "build zero vector" in {
      Zero(3)() must_== Vector(0.0, 0.0, 0.0)
    }

    "build 'random' vector" in {
      val sut = RandomCube(3, 314159)()
      sut forall (abs(_) <= 1.0)
    }
  }
  
  "Vector object" should {
    "calculate average" in {
      Vector.average(Array(Vector(1.0, 2.0, 3.0), Vector(-1.0, -1.0, -1.0))) must_==
        Vector(0.0, 0.5, 1.0)
    }
  }
}
