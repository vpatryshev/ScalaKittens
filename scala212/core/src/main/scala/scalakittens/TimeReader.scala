package scalakittens

/**
  * Created by vpatryshev on 6/19/16.
  */
trait TimeReader {
  def currentTime: Long
}

object DateAndTime extends TimeReader {
  def currentTime: Long = System.currentTimeMillis()
}