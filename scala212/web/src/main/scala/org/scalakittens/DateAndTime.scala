package org.scalakittens

import java.text.SimpleDateFormat
import java.util.{Date, SimpleTimeZone, TimeZone}

import org.scalakittens.DateAndTime._
import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}

import scala.concurrent.duration.Duration
import scala.language.postfixOps
import scala.util.matching.Regex

/**
  * date/time management
  */

trait TimeReader {

  def currentTime: Long = System.currentTimeMillis

  def flowOfTime: Iterator[Long] = Iterator.iterate(currentTime){_ ⇒ currentTime}

  def interval(ttl: Long): Iterator[Long] = {

    flowOfTime.next // the first drop, may be stale
    val end = flowOfTime.next + ttl
    flowOfTime takeWhile (end >)
  }

  def interval(ttl: Duration): Iterator[Long] = interval(ttl.toMillis)

  private[scalakittens] def int(s: String) = s.trim.dropWhile(0==).toInt

  def now: java.sql.Timestamp = new java.sql.Timestamp(currentTime)

  implicit class RichTimestamp(duration: Duration) {
    def ago = new java.sql.Timestamp(currentTime - duration.toMillis)
  }

  def minTimestamp: Long = currentTime - 20 * MILLIS_IN_A_YEAR // approximately 20 years ago

  def earliestRecentDate = new Date(minTimestamp)

  def maxTimestamp: Long = currentTime + MILLIS_IN_A_YEAR // 1 year ahead, do we need this date?

  def today = new java.util.Date(currentTime)

  lazy val EasternTimeZone: TimeZone = TimeZone.getTimeZone("EST")

  lazy val UTC = new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC")


  def usingTimezone: SimpleTimeZone = UTC

  def jodaNow = new DateTime(now, JodaUTC)

  def Today: Date = new DateTime(now, JodaUTC).toDate

  def jodaToday: DateTime = jodaNow.withHourOfDay(12).withMinuteOfHour(0).withSecondOfMinute(0).withMillisOfSecond(0)

  def jodaTomorrow: DateTime = jodaNow.plusDays(1).withHourOfDay(12).withMinuteOfHour(0).withSecondOfMinute(0).withMillisOfSecond(0)

  def tomorrow: Date = jodaTomorrow.toDate

  private[scalakittens] def newToday = jodaToday

  def threeDaysAgo: DateTime = lastMidnight minusDays 2

  def lastMidnight: DateTime = jodaNow.withHourOfDay(0).withMinuteOfHour(0).withSecondOfMinute(0).withMillisOfSecond(0)

  def yesterday: DateTime = jodaToday.minusDays(1)

  def lastYear: DateTime = jodaToday.minusYears(1).withMonthOfYear(1).withDayOfMonth(1)

  def monthsAgo(n: Int): DateTime = jodaToday.minusMonths(n)

  def yearAgo: DateTime = monthsAgo(12)

  def GetYesterdayString: String = formatter.format(yesterday.toDate.getTime)

  def todayString: String = formatter.format(today)

  def todayShortString: String = shortFormatter.format(currentTime)

  def lastYearJan1String: String = formatter.format(lastYear.toDate.getTime)

  def LastYearNumberAsString: String = lastYear.getYear.toString

  def yearAgoString: String = formatter.format(yearAgo.toDate.getTime)

  def yearAgoShortString: String = shortFormatter.format(yearAgo.toDate.getTime)

  // Sets proper time (Noon)
  def stringToValidDate(format:  String,
                        whatIsIt:String = "Date",
                        earliest:Long   = minTimestamp,
                        latest  :Long   = maxTimestamp): String ⇒ Result[Date] = (s: String) ⇒ {
    val parsed = parseDate(format)(s)
    val result = parsed filter ((d:Date) ⇒ {
      val t = d.getTime
      t > earliest && t < latest}, (d:Date) ⇒ s"$whatIsIt out of range: $s (${timeToString(earliest)} - ${timeToString(latest)})")
    result
  }

  def ymdNow: String = dbDate(now)

  def hmsNow: String = hmsFormatter.format(now)

}

case class DateFormat(fmt: String) {
  lazy val parseCurrent: String ⇒ Result[Date] = s ⇒ stringToValidDate(fmt, "Date/time")(s)

  def isValid(s: String): Boolean = parseCurrent(s).isGood
}

trait DateAndTime {
  def TooEarlyDate: Long = -2140704000000L
  def MILLIS_IN_A_DAY: Long = 1000L * 60 * 60 * 24
  def MILLIS_IN_A_YEAR: Long = 366 * MILLIS_IN_A_DAY
  val beforeHEx = new DateTime(2000, 1, 1, 0, 0, 0, DateTimeZone.UTC)

  // Specify date format
  def parseFormattedDate(dFormat:String)(s:String): Result[java.util.Date] =
  {
    val d = new SimpleDateFormat(dFormat)
    d.setLenient(false) // strict
  val dt = Result.forValue(d.parse(s), s"Date format error: $s")
    dt
  }

  def dateOption(s: String): Option[java.util.Date] = extractDate(s).toOption

  // May not set proper time (Noon) if time is present
  def extractDate(s:String)(implicit zone: TimeZone = UTC): Result[java.util.Date] =
  {
    val r0 = Result.forValue(s) map (_.trim)
    val r1: Result[String] = r0 filter (!_.isEmpty)
    val filtered = r1 orCommentTheError "date missing"

    def fromMMMddyyyy(sm:String,sd:String,sy:String) =
      if (months.contains (sm take 3 toLowerCase))
        Good(months(sm take 3 toLowerCase)+1, int(sd), int(sy), 12, 0, 0, 0)
      else Result.error(s"Failed to parse date")

    def fromMMddyy(sm:String, sd: String, sy: String) = {
      val y0 = int(sy)
      val y = if (y0 < Y2K) y0+2000 else if (y0 < 1900) y0+1900 else y0
      Good((int(sm),int(sd),y, 12, 0, 0, 0))
    }

    // Sets proper time (Noon)
    def fromMMMyy(sm:String, sy: String) = {
      if (months.contains (sm take 3 toLowerCase))
        Good(months(sm take 3 toLowerCase)+1, int("01"), int("20"+sy), 12, 0, 0, 0)
      else Result.error(s"Failed to parse date")
    }

    /**
      * This is a special kind of format, we don't know whether it is
      * mmddyy or yymmdd
      *
      * Sets proper time (Noon)
      * @param smORy this may be month, or may be year, nobody knows
      * @param sd day of the month
      * @param syORm this may be month, or may be year, nobody knows
      * @return sextuples: m,d,y,h,m,s
      */
    def fromXxyyzz(smORy:String, sd: String, syORm: String) = {
      val y0 = int(syORm)
      val m0 = int(smORy)
      if (y0 > 0 && y0 < 13 && m0 > 0 && m0 < 13) Result.error(s"Could not figure out whether it is $m0/$sd/20$y0 or $y0/$sd/20$m0")
      val (m1,y1) = if (y0 == 0 || y0 > 12) (m0,y0) else (y0,m0)
      val y = if (y1 < Y2K) y1+2000 else if (y1 < 1900) y1+1900 else y1
      Good((m1,int(sd),y, 12, 0, 0, 0))
    }

    def fromMMddyyyy(sm:String, sd: String, sy: String) = Good((int(sm),int(sd),int(sy), 12, 0, 0, 0))

    def fromyyyMMddhms(sm:String, sd: String, sy: String, sh: String, smin: String, ssec: String, fraction: Option[String] = Some("0")) = {
      val smsi = fraction map(f ⇒ int(f.tail + "000" take 3)) getOrElse 0
      Good((int(sm),int(sd),int(sy), int(sh), int(smin), int(ssec), smsi))
    }

    filtered flatMap { s ⇒ // ignore cleanup!!!!!
      //        val sDate = if (!cleanUp) s else s.replaceAll("[^0-9\\/]","")
      val mdyhms:Result[(Int,Int,Int, Int, Int, Int, Int)] = s.trim match {
        case YyyyMMdd(sy,sm,sd)  ⇒ fromMMddyyyy(sm, sd, sy)
        case MMddyyyy(sm,sd,sy)  ⇒ fromMMddyyyy(sm, sd, sy)
        case Mmddyyyy(sm,sd,sy)  ⇒ fromMMddyyyy(sm, sd, sy)
        case MMddyy  (sm,sd,sy) ⇒ fromMMddyy(sm, sd, sy)
        case xxyyzz  (sm,sd,sy) ⇒ fromXxyyzz(sm, sd, sy)
        case MMMddyyyy(sm,sd,sy) ⇒ fromMMMddyyyy(sm,sd,sy)
        case MMMyy(sm,sy) ⇒ fromMMMyy(sm,sy)
        case FullDate(sm,sd,sy)  ⇒ fromMMMddyyyy(sm,sd,sy)
        case ISODateAndTimeSplit(sy,sm,sd, sh, smin, ssec)  ⇒ fromyyyMMddhms(sm,sd,sy, sh, smin, ssec)
        case PostgresDateAndTime(sy,sm,sd, sh, smin, ssec,fraction) ⇒ fromyyyMMddhms(sm,sd,sy, sh, smin, ssec, Option(fraction))
        case _                   ⇒  Result.error(s"Bad date format: [[$s]]")
      }

      mdyhms flatMap {
        case(m,d,y,h,min,sec, ms) ⇒
          val date = datetime(y, m, d, h, min, sec, ms)
          date
         // joda.map(_.toDate)
      }
    }
  }

  def date(y:Int, m: Int, d: Int): Result[Date] = datetime(y,m,d,12,0,0)

  def datetime(y:Int, m: Int, d: Int, h: Int, min: Int, s: Int, ms: Int = 0): Result[Date] =
    jodaDatetime(y,m,d,h,min,s,ms) map (_.toDate)

  def jodaDatetime(y:Int, m: Int, d: Int, h: Int, min: Int, s: Int, ms:Int = 0): Result[DateTime] = Result.forValue {
    val jodaTime = new DateTime(y, m, d, h, min, s, ms, DateTimeZone.UTC)
    jodaTime
  }

  val JodaUTC: DateTimeZone = DateTimeZone.UTC

  // Sets proper time (Noon)
  def parseDate(format: String, zone: DateTimeZone = JodaUTC): String ⇒ Result[Date] = {
    val formatter = DateTimeFormat.forPattern(format).withZone(zone)
    s0: String ⇒ {
      val s = s0.trim
      if (Strings.isEmpty(s)) {
        Result.error("Date missing")
      } else Result.forValue(formatter.parseDateTime(s)) map (d ⇒ {
        val date = d.plusHours(12).toDate
        date
      })
    }
  }


  /// Date util
  def SubtractDaysFromDate(d:java.util.Date, days:Long):java.util.Date =
  {
    val dt = d.getTime
    new java.util.Date( dt-(days*MILLIS_IN_A_DAY) )
  }

  def optionalTimeToDateString(timestamp: Option[Long]):String = timestamp.fold("(Unknown)")(t ⇒ shortDate(new Date(t)))

  val formatter = new SimpleDateFormat("MM/dd/yyyy")

  def dateToString(date: DateTime): String = formatter.format(date.toDate)

  def dateToString(date: Date): String = formatter.format(date)

  def timeToString(time: Long): String = dateToString(new Date(time))

  val shortFormatter = new SimpleDateFormat("MM/dd/yy")

  val dbFormatter = new SimpleDateFormat("yyyy-MM-dd")

  def jodaShortDate(date: DateTime): String = shortDate(date.toDate)

  def shortDate(date: Date): String = shortFormatter.format(date)

  def dbDate(date: DateTime): String = dbDate(date.toDate)

  def dbDate(date: Date): String = dbFormatter.format(date)

  val hmsFormatter = new SimpleDateFormat("HH-mm-ss")
}

object DateAndTime extends DateAndTime with TimeReader {

  def isRecent(d: Date): Boolean = d.getTime > minTimestamp

  val MMddyy: Regex ="""(\d{1,2})/ ?(\d{1,2})/(\d{2,2})\b.*""".r
  val xxyyzz: Regex ="""(\d{1,2})-(\d{1,2})-(\d{2,2})\b.*""".r
  val MMddyyyy: Regex ="""(\d{1,2})[\/-](\d{1,2})[\/-](\d{4}).*""".r
  val Mmddyyyy: Regex ="""(\d{2})(\d{2})(\d{4}).*""".r
  val MMMddyyyy: Regex ="""(\w{3,}) (\d{1,2}),? (\d{4})""".r
  val YyyyMMdd: Regex = """(\d{4})-(\d{2,2})-(\d{2,2})""".r
  val MMMyy: Regex = """(\w{3,})-(\d{2})""".r
  //Sun Mar 31 00:00:00 EST 1985
  val FullDate: Regex ="""....(\w{3,}) (\d{1,2}) ..:..:.. ... (\d{4})""".r
  val FullDateAndTime: Regex ="""....(\w{3,}) (\d{1,2}) (\d{1,2}):..:.. ... (\d{4})""".r
  val d4 = """\d{4}"""
  val d2 = """\d{2}"""
  val ISODateAndTime: Regex =s"$d4-$d2-$d2-$d2\\.$d2\\.$d2".r
  val ISODateAndTimeSplit: Regex = s"($d4)-($d2)-($d2)-($d2)\\.($d2)\\.($d2)".r
  val PostgresDateAndTime: Regex =s"($d4)-($d2)-($d2) ($d2):($d2):($d2)(\\.\\d+)?".r
  val months: Map[String, Int] = "jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec" .split (",") .zipWithIndex .toMap

  val Y2K = 20 // years before 20 are ascribed to 21st century, years after - to 20th century

  def time[T](op: ⇒ T): T = {
    val now = System.nanoTime()
    try {
      val res = op
      res
    } finally {
      val dt = System.nanoTime() - now
      // for debugging/profiling
      //      Logging.debug(s"Operation took " + DateAndTime.formatNano(dt))
    }
  }

  def time[T](name: String, op: ⇒ T): T = {
    val now = System.nanoTime()
    val res = op
    val dt = System.nanoTime() - now
    // (good for profiling/debugging)
    //  println(s"$name took " + DateAndTime.formatNano(dt))
    res
  }

  import java.text._
  val SmallFmt = new DecimalFormat("0.000")
  def formatNano(nano: Long): String = {
    val mks = nano / 1000
    val ms = nano / 1000000
    val s = nano / 1000000000L
    val m = nano / 60000000000L
    val h = nano / 3600000000000L

    if (mks < 50) SmallFmt.format(nano * 0.001) + "mks"
    else if (ms  < 50) SmallFmt.format(mks * 0.001) + "ms"
    else if (s   < 50) SmallFmt.format(ms * 0.001) + "sec"
    else if (m   < 10) s"${m}m ${s%60}s"
    else if (h   <= 0) s"${m}m"
    else               s"${h}h ${m%60}m"
  }
}

