package scalakittens.stats

import org.specs2.mutable.Specification

import scalakittens.la.VectorSpace

/**
  * Tests for statistics
  * Created by vpatryshev on 5/7/17.
  */
class StatsTest extends Specification {
  val R3 = VectorSpace(3)
  val R4 = VectorSpace(4)
  
  "AccumulatingMoments" should {
    "calculate moments" in {
      val moments = new AccumulatingMoments(R3)
      val averager = moments.collect(List(R3.Vector(1.0, 2.0, 3.0), R3.Vector(-1.0, -1.0, -1.0)))
      
      averager.avg must_== R3.Vector(0.0, 0.5, 1.0)
      averager.n must_== 2
    }
    
    "calculate covariance" in {
      val vectors = List(
        R4.Vector(1.0, 3.0, -.5, 1.0), // -1, -3, 0.5, 1/3
        R4.Vector(2.0, 6.0, -1.0, 0.0), // 0, 0, 0, -2/3
        R4.Vector(3.0, 9.0, -1.5, 1.0)) // 1, 3, -0.5, 1/3

      val moments = new AccumulatingMoments(R4)
      val averager = moments.collect(vectors)
      val cov = averager.covariance

      val expected = Array(
        Vector(1.0, 3.0, -0.5, 0.0),
        Vector(3.0, 9.0, -1.5, 0.0),
        Vector(-0.5, -1.5, 0.25, 0.0),
        Vector(0.0, 0.0, 0.0, 0.33333333333333337)
      )

      for {
        i <- 0 until 4
      } cov.row(i) aka s"@$i" must_== expected(i)
      
      ok
    }
  }
}
