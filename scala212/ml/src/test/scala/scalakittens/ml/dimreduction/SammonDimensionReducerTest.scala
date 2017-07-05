package scalakittens.ml.dimreduction

import org.specs2.mutable.Specification

import scalakittens.la.Spaces.R3
import math._
import scalakittens.la.Norm
import scalakittens.ml.TestTools._

/**
  * Created by vpatryshev on 7/3/17.
  * see also https://lvdmaaten.github.io/tsne/
  */
class SammonDimensionReducerTest extends Specification {
  
  def buildTestFigure: Array[R3.Vector] = {
    val step: Double = 0.025
    
    def wingPointAt(alpha: Double, beta: Double) = {
      R3.Vector(cos(alpha)*(1+cos(beta)), sin(alpha)*(1+cos(beta)), sin(beta))
    }
    
    def wing(alpha: Double, from: Double, to: Double): Array[R3.Vector] = {
      val wingSize = (abs(from - to) / step).toInt
      val out = new Array[R3.Vector](wingSize)
      for {
        i <- 0 until wingSize
      } {
        val beta = (to - from)*i/(wingSize-1)
        out(i) = wingPointAt(alpha, from + beta)
      }
      out
    }
    
    def connector(alpha1: Double, alpha2: Double, beta: Double): Array[R3.Vector] = {
      val from = wingPointAt(alpha1, beta)
      val to = wingPointAt(alpha2, beta)
      val z = from(2)
      val midPoint = (from + to) / 2
      val center = R3.Vector(midPoint(0) + (to(1)-from(1))/2, midPoint(1) - (to(0)-from(0))/2, z)
      val r = Norm.l2(center - from)
      val size = (Pi/4/step*r).toInt - 1
      val out = new Array[R3.Vector](size)

      for {
        i <- 0 until size
      } {
        val gamma = -Pi/2 + alpha1 - (Pi/2)*(i+1)/size
        out(i) = R3.Vector(center(0) + r * cos(gamma), center(1) + r * sin(gamma), z)
      }
      out
    }
    
    val wp0 = wingPointAt(0, -Pi*3/4)
    val wp1 = wingPointAt(0, Pi*3/4)
    val wp2 = wingPointAt(Pi/2, Pi*3/4)
    val w0 = wing(0, -Pi*3/4, Pi*3/4)
    val w1 = wing(Pi/2, Pi*3/4, -Pi*3/4)
    val w2 = wing(Pi, -Pi*3/4, Pi*3/4)
    val w3 = wing(Pi*3/2, Pi*3/4, -Pi*3/4)
    val c01 = connector(0, Pi/2, Pi*3/4)
    val wp1n = w1.last
    val vp20 = w2.head
    val c12 = connector(Pi/2, Pi, Pi*5/4)
    val c23 = connector(Pi, Pi*3/2, Pi*3/4)
    val c34 = connector(Pi*3/2, Pi*2, Pi*5/4)
    val res = w0 ++ c01 ++ w1 ++ c12 ++ w2 ++ c23 ++ w3 ++ c34

    res
  }
  
  "Test figure" should {
    "be ok" in {
      val points: List[R3.Vector] = buildTestFigure.toList
      val toView1: List[(Int, Double, Double)] = points.map ((v:R3.Vector) => {
        ((v.apply(2)*100).toInt, v.apply(0), v.apply(1))
      })
        
        visualize("Horizontal", toView1)

      val toView2: List[(Int, Double, Double)] = points.map ((v:R3.Vector) => {
        ((v.apply(0)*100).toInt, v.apply(1), v.apply(2))
      })

      visualize("Vertical1", toView2)

      val toView3: List[(Int, Double, Double)] = points.map ((v:R3.Vector) => {
        ((v.apply(1)*100).toInt, v.apply(0), v.apply(2))
      })

      visualize("Vertical2", toView3)
      ok
    }
  }
  
  "SammonDimensionReducerTest" should {
    "reduce" in {
      ok
    }

    "withPCA" in {
      ok
    }

  }
}
