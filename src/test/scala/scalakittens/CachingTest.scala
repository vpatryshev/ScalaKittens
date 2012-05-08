package scalakittens

import java.io.{StringWriter, Writer}
import Caching._
import org.specs.Specification
import org.specs.runner.{ConsoleRunner, JUnit4}
import java.util.concurrent.TimeUnit

class CachingTest extends JUnit4(CachingTest)

object CachingTestRunner extends ConsoleRunner(CachingTest)

object CachingTest extends Specification {
  var myTime: Long = 0

  val sut = new Caching {
    def now = myTime
  }
  implicit def currentValue[T](cu: CacheUnit[T]): T = cu.apply

  "A Cache" should {
    "return the right stuff" in {
      var i: Int = 0
      val cached = sut.cache(() => {i}).validFor(3).NANOSECONDS
      i = 17
      val n1: Int = cached()
      n1 must_== 17
      i = 18
      val n2: Int = cached()
      n2 must_== 17
      myTime = 2
      val n3: Int = cached()
      n3 must_== 17
      myTime = 3
      val n4: Int = cached()
      n4 must_== 18
      i = 19
      myTime = 10
      val n5: Int = cached()
      n5 must_== 19
    }

    "only call function once per period" in {
      val mutex = new Object
      def pause = mutex.synchronized(mutex.wait)
      def signal = mutex.synchronized(mutex.notifyAll)
      val numTries = 15

      var counter: Char = 'a'
      val cached = sut.cache(() => { counter = (counter+1).toChar; counter}) validFor 10 HOURS

      def startConsuming(out: Writer) = new Thread {
        override def run {
          (1 to numTries) foreach ( (i:Int) => { out.write(cached()); pause } )
        }
      }.start

      val buffers = ((1 to 2000) map ((i:Int) => new StringWriter)).toList
      val TEN_HOURS = TimeUnit.HOURS toNanos 10

      buffers foreach startConsuming

      Thread sleep 20

      for (i <- 1 to numTries) {
        myTime = myTime + TEN_HOURS + 1
        signal
        Thread sleep 100
      }
      buffers foreach (_.toString must_== "bcdefghijklmnop")
    }
  }
}