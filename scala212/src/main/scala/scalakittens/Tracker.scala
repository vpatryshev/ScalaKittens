package scalakittens

/**
  * Created by vpatryshev on 7/21/17.
  */

class Tracker {
  private var t: Long = System.nanoTime
  def << (s: String): Unit = {
    val t1 = System.nanoTime
    val dtmks = (t1 - t + 500)/1000
    println(if (dtmks < 10000) 
      s"Spent $dtmks us: $s"
    else s"Spent ${(dtmks+500)/1000} ms: $s")

    t = t1
  }
}
