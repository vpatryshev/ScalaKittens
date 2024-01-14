package scalakittens.stats

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.languageFeature.postfixOps
import scalakittens.{DateAndTime, TimeReader}

/**
  *
  * Created by vpatryshev on 12/25/15.
  */
class TimeSeries(range: Duration, clock: TimeReader = DateAndTime) {
  private val data = new ListBuffer[(Duration, Double)]()

  private def cleanup(minTime: Duration): Unit = {
    data.synchronized {
      while (data.nonEmpty && data.head._1 < minTime) data.remove(0)
    }
  }

  def t0: Duration = clock.currentTime millis

  def add(value: Double): Unit = {
    data.append((t0, value))
    cleanup(t0 - range)
  }

  def last = data.lastOption

  def lastN(n: Int) = data.takeRight(n)

  def latest(Δt: Duration): Seq[Double] = {
    val t1: Duration = t0 - Δt
    val ourData = data.dropWhile(_._1 < t1)
    ourData map (_._2)
  }

  def moments(Δt: Duration): MomentData = (MomentData.Zero /: latest(Δt)) ((m: MomentData, x: Double) => m add x)

  def avg(Δt: Duration): Option[Double] = moments(Δt).avg

  def σ(Δt: Duration): Option[Double] = moments(Δt).σ
}
