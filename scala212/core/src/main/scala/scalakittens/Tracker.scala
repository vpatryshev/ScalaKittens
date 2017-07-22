package scalakittens

/**
  * Created by vpatryshev on 7/21/17.
  */

class Tracker {
  private var t: Long = System.currentTimeMillis
  def << (s: String): Unit = {
    val t1 = System.currentTimeMillis
    println(s"Spent ${t1-t} ms: $s")
    t = t1
  }
}
