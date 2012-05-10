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
    def now = myTime // using cake pattern to mock time
  }
  implicit def currentValue[T](cu: CacheUnit[T]): T = cu.apply()

  "A Cache" should {
    "return the right stuff" in {
      var i: Int = 0 // boring
      val cached = sut.cache(() => {i}).validFor(3).NANOSECONDS
      i = 17
      val n1: Int = cached()
      n1 must_== 17
      i = 18
      val n2: Int = cached()
      n2 must_== 17
      myTime = 2 // still valid eh
      val n3: Int = cached()
      n3 must_== 17
      myTime = 3 // now it's stale, and will update
      val n4: Int = cached()
      n4 must_== 18 // yesss!!!
      i = 19
      myTime = 10
      val n5: Int = cached() // one more time, to make sure it's not stuck
      n5 must_== 19
    }

    "only call function once per period" in {
      val mutex = new Object // used in controlling the 20k threads
      def pause  { mutex.synchronized(mutex.wait) }
      def signal { mutex.synchronized(mutex.notifyAll) }
      val numTries = 15

      var counter: Char = 'a'
      val cached = sut.cache(() => { counter = (counter+1).toChar; counter}) validFor 10 HOURS

      def startConsuming(out: Writer) = new Thread {
        override def run { // this thread will
          (1 to numTries) foreach ( // repeat this number of times
            (i:Int) => { out.write(cached()) // log the current entry of cache
                         pause } ) // and wait for a signal
        }
      }.start // yeah, launch it

      val buffers = ((1 to 20000) map ((i:Int) => new StringWriter)).toList // have so many buffers
      val TEN_HOURS = TimeUnit.HOURS toNanos 10 // an arbitrary timeout value

      buffers foreach startConsuming // right, we start so many threads, one per buffer

      Thread sleep 200 // give them all a chance to reach the gate

      for (i <- 1 to numTries) { // now control our threads, they are waiting
        myTime = myTime + TEN_HOURS + 1 // time's up for a value
        signal // tell them all to consume - only one is supposed to bump the counter
        Thread sleep 1000 // give them a chance to do their job
      }
      buffers foreach (_.toString must_== "bcdefghijklmnop") // this is the ideal we are looking for
    }
  }
}