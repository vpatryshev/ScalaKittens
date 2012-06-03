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

    val mutex = new Object // used in controlling the 20k threads
    def pause  { mutex.synchronized(mutex.wait) }
    def signal { mutex.synchronized(mutex.notifyAll) }
    val numTries = 9

    var counter: Char = 'a'

    def startConsuming(cached: sut.CacheUnit[Char])(out: Writer) = new Thread {
      override def run { // this thread will
        (1 to numTries) foreach ( // repeat this number of times
          (i:Int) => { out.write(cached()) // log the current entry of cache
                       pause } ) // and wait for a signal
      }
    }.start // yeah, launch it

    "only call function once per period" in {
      val cached = sut.cache(() => { counter = (counter+1).toChar; counter}).validFor(10).HOURS
      counter = 'a'
      val numThreads: Int = 10000
      val buffers = ((1 to numThreads) map ((i:Int) => new StringWriter)).toList // have so many buffers
      val TEN_HOURS = TimeUnit.HOURS toNanos 10 // an arbitrary timeout value

      buffers foreach startConsuming(cached) // right, we start so many threads, one per buffer

      Thread sleep 200 // give them all a chance to reach the gate

      for (i <- 1 to numTries) { // now control our threads, they are waiting
        myTime = myTime + TEN_HOURS + 1 // time's up for a value
        signal // tell them all to consume - only one is supposed to bump the counter
        Thread sleep 10000 // give them a chance to do their job
      }

      buffers foreach (_.toString must_== "bcdefghij") // this is the ideal we are looking for
    }

    "try the same with soft references" in {
      val cached = sut.cache(() => { counter = (counter+1).toChar; counter}).withSoftReferences.validFor(5).MINUTES
      counter = 'a'
      val numThreads: Int = 5000
      val buffers = ((1 to numThreads) map ((i:Int) => new StringWriter)).toList
      val FIVE_MINUTES = TimeUnit.MINUTES toNanos 7

      buffers foreach startConsuming(cached)

      Thread sleep 200

      for (i <- 1 to numTries) {
        myTime = myTime + FIVE_MINUTES + 1
        signal
        Thread sleep 1000
      }
      buffers foreach (_.toString must_== "bcdefghij")
    }
  }
}