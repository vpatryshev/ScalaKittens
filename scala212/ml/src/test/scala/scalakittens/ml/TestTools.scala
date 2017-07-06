package scalakittens.ml

import language.postfixOps
import scala.math.{Pi, abs, cos, sin}
import scalakittens.la.Norm
import scalakittens.la.Spaces.R3

/**
  * Tools for ml testing.
  * 
  * Created by vpatryshev on 7/3/17.
  */
object TestTools {
  val N = 120 /*120*/
  val M = 30 /*60*/
  def visualize(title: String, projections: List[(Any, Double, Double)]): Unit = {
    println
    println
    println("=" * N)
    println(s"                                    $title\n")
    println("-" * N)
    val xs = projections.map(_._2)
    val ys = projections.map(_._3)

    val (xmin, xmax) = (xs.min, xs.max)
    val (ymin, ymax) = (ys.min, ys.max)

    val xScale = (xmax - xmin) / N
    val yScale = (ymax - ymin) / M

    val sample = projections map {
      case (w, x, y) => (w, ((x - xmin) / xScale).toInt, ((y - ymin) / yScale).toInt)
    }

    val samplesByLine = sample.groupBy(_._3)

    0 to M foreach {
      j =>
        val row = samplesByLine.getOrElse(j, Nil) map (t => t._1 -> t._2)

        val layout = (Map[Int, Char]() /: row) {
          case (charMap, (z, pos)) =>
            val w = z.toString
            val wordRange = math.max(pos - 1, 0) until math.min(N, pos + w.length + 1)
            if (wordRange exists charMap.contains) charMap else {
              val m1 = 0 until w.length map (i => i + pos -> w.charAt(i)) toMap

              charMap ++ m1
            }
        }

        val chars = 0 to N map (layout.getOrElse(_, ' '))

        print(chars mkString)
        println
    }
  }

  def testFigure3dButterfly: Array[R3.Vector] = {
    val precision: Double = 0.025

    def wingPointAt(alpha: Double, beta: Double) = {
      R3.Vector(cos(alpha)*(1+cos(beta)), sin(alpha)*(1+cos(beta)), sin(beta))
    }

    def wing(alpha: Double, from: Double, to: Double): Array[R3.Vector] = {
      val wingSize = (abs(from - to) / precision).toInt
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
      val size = (Pi/4/precision*r).toInt - 1
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

}
