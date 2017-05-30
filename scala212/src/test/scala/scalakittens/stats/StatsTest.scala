package scalakittens.stats

import org.specs2.mutable.Specification

import scalakittens.la.Vector

/**
  * Tests for statistics
  * Created by vpatryshev on 5/7/17.
  */
class StatsTest extends Specification {

  
  "AccumulatingMoments" should {
    "calculate moments" in {
      val averager = AccumulatingMoments(3).collect(List(Vector(1.0, 2.0, 3.0), Vector(-1.0, -1.0, -1.0)))
      
      averager.avg must_== Vector(0.0, 0.5, 1.0)
      averager.n must_== 2
    }
    
    "calculate covariance" in {
      val vectors = Array(
        Vector(1.0, 3.0, -.5, 1.0), // -1, -3, 0.5, 1/3
        Vector(2.0, 6.0, -1.0, 0.0), // 0, 0, 0, -2/3
        Vector(3.0, 9.0, -1.5, 1.0)) // 1, 3, -0.5, 1/3

      val averager = AccumulatingMoments(4).collect(vectors)
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
