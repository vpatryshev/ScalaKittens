package scalakittens.stats

import org.specs2.mutable.Specification

import scalakittens.la.Vector

/**
  * Tests for statistics
  * Created by vpatryshev on 5/7/17.
  */
class StatsTest extends Specification {

  
  "AccumulatingMoments" should {
    "apply to vectors" in {
      import math._

      val averager = AccumulatingMoments(3).update(List(Vector(1.0, 2.0, 3.0), Vector(-1.0, -1.0, -1.0)))
      
      averager.avg must_== Vector(0.0, 0.5, 1.0)
      averager.n must_== 2
      
    }
  }
}
