package scalakittens.ml.word2vec

import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

/**
  * Created by vpatryshev on 5/9/17.
  */
class SigmaTest extends Specification {
  import math._

  "Σ" should {
    "provide precision" in {

      def checkOn(dy: Double, size: Int): Unit = {
        val sut = new Sigma { def ε = dy }
        sut.size < size aka s"?? ${sut.size} vs $size" must beTrue
        sut.table.length < size aka s"?? ${sut.table.length}" must beTrue
        sut.σ(0.0) aka ("oops: " + (sut.table mkString ",")) must_== 0.5
        abs(sut.σ(-1.945) - 0.125) < dy aka s"with max ${sut.max} dy=$dy got ${sut.σ(-1.945)} with table argument ${sut.δ/2+1.945/sut.δ}\ntable ${sut.table mkString ","}" must beTrue
        
        for (i <- 1 until sut.size) {
          val y0 = sut.table(i-1)
          val y1 = sut.table(i)
          y0 > y1    aka s"1. @$i: expected $y0 > $y1" must beTrue
          y1 > y0-dy aka s"2. @$i: expected $y0 < $y1 + ${sut.δ}" must beTrue
        }
        
        def checkPrecision(x: Double): MatchResult[Any] = {
          val y0 = 1 / (1+exp(-x))
          val y1 = sut.σ(x)
          abs(y1 - y0) < dy aka s"$x for $dy, size=$size, expected $y0, got $y1" must beTrue
          ok
        }
        
        for (i <- -10000 until 10000) checkPrecision(i.toDouble / 1000)
      }

      checkOn(0.125, 15)
      checkOn(0.01, 230)
      checkOn(0.001, 4000)
      checkOn(0.0001, 42000)
      ok
    }
  }

}
