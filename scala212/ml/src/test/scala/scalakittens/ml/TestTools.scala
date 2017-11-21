package scalakittens.ml

//import language.postfixOps
import scala.math.{Pi, abs, cos, sin}
import scalakittens.la.Spaces.R3

/**
  * Tools for ml testing.
  * 
  * Created by vpatryshev on 7/3/17.
  */
object TestTools {

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

    def connector(alpha1: Double, beta: Double): Array[R3.Vector] = {
      val alpha2 = alpha1 + Pi/2
      val from = wingPointAt(alpha1, beta)
      val to = wingPointAt(alpha2, beta)
      val z = from(2)
      val midPoint = (to + from) / 2
      val delta = (to - from)/2
      val center = R3.Vector(midPoint(0) + delta(1), midPoint(1) - delta(0), z)
      val r = math.abs(1+cos(Pi*3/4))
      val size = (Pi/4/precision*r).toInt - 1
      val out = new Array[R3.Vector](size)

      for {i <- 0 until size} {
        val gamma = -Pi/2 + alpha1 - (Pi/2)*(i+1)/size
        out(i) = R3.Vector(center(0) + r * cos(gamma), center(1) + r * sin(gamma), z)
      }
      out
    }

    val w0 = wing(0, -Pi*3/4, Pi*3/4)
    val w1 = wing(Pi/2, Pi*3/4, -Pi*3/4)
    val w2 = wing(Pi, -Pi*3/4, Pi*3/4)
    val w3 = wing(Pi*3/2, Pi*3/4, -Pi*3/4)
    val c01 = connector(0, Pi*3/4)
    val c12 = connector(Pi/2, Pi*5/4)
    val c23 = connector(Pi, Pi*3/4)
    val c34 = connector(Pi*3/2, Pi*5/4)
    val res = w0 ++ c01 ++ w1 ++ c12 ++ w2 ++ c23 ++ w3 ++ c34
    res
  }

}
