package scalakittens.stats

import scala.concurrent.duration._
import scala.languageFeature.postfixOps
import scalakittens.{DateAndTime, TimeReader}

/**
  * Created by vpatryshev on 12/25/15.
  */
class Stats(clock: TimeReader = DateAndTime) {
  private val data = new TimeSeries(1 day, clock)

  def last = data.last map (_._2)

  def add(x: Double) = data.add(x)

  def lastDay = data.moments(1 day)

  def lastHour = data.moments(1 hour)

  def lastMinute = data.moments(1 minute)

  override def toString = s"last=${last map (""+_) getOrElse "(missing)"}, lastMinute=($lastMinute), lastHour=($lastHour), perDay=($lastDay)"
}
