package scalakittens

import java.io.{StringWriter, Writer}
import Caching._
import org.specs2.Specification
import java.util.concurrent.TimeUnit
/*
class CachingTest extends JUnit4(CachingTest)

object CachingTestRunner extends ConsoleRunner(CachingTest)

object CachingTest extends Specification {
  var myTime: Long = 0

  def timePls = {
    val s = myTime.toString
    if (s.length < 7) s else (s.substring(0, s.length - 6) + s.substring(s.length - 6))
  }
var patience = 10
  val sut = new Caching {
    def now = synchronized { myTime } // using cake pattern to mock time
  }
  implicit def currentValue[T](cu: CacheUnit[T]): T = cu.apply()

  "A Cache" should {
    "return the right stuff" in {
      var i: Int = 0 // boring
      val cached = sut.cache(() ⇒ i).validFor(3).NANOSECONDS
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

    val mutex = new Object {
      def pause { synchronized  (wait) }
      def signal { synchronized (notifyAll) }
    }// used in controlling the 20k threads
    def pause  { mutex.pause }
    def signal { mutex.signal }
    val numTries = 9

    var counter: Char = 'a'

    def startConsuming(cached: sut.CacheUnit[Char])(out: Writer) = {
      val t = new Thread {
        override def run() { // this thread will
          (1 to numTries) foreach ( // repeat this number of times
            (i:Int) ⇒ {
              val value: Char = cached()
              sut.debug("attempt#" + i + "⇒" + value);
              out.write(value) // log the current entry of cache
              pause } ) // and wait for a signal
        }
      }
      t.start()
      t
    }

    var threads: Seq[Thread] = Nil

    "only call function once per period" in {
      synchronized {myTime = 0}
      def runThreads(numThreads: Int): Boolean = {
        println("-- starting with " + numThreads + " threads --")
        counter = 'a'
        val TTL = TimeUnit.HOURS toNanos 10 // an arbitrary timeout value
        val cached = sut.cache(() ⇒ { counter = (counter+1).toChar; debug("tick:" + counter); counter}).validFor(10).HOURS
        val buffers = ((1 to numThreads) map ((i: Int) ⇒ new StringWriter)).toList // have so many buffers

        threads foreach (_.stop)
        threads = buffers map startConsuming(cached) // right, we start so many threads, one per buffer

        Thread sleep 200 // give them all a chance to reach the gate
        debug("*** Time is supposed to be 0: " + myTime + "***")
        for (i <- 1 to numTries) {
          // now control our threads, they are waiting
          synchronized {myTime = myTime + TTL + 1} // time's up for a value
          debug("(attempt=" + i + ")")
          signal // tell them all to consume - only one is supposed to bump the counter
          Thread sleep 1000//0 // give them a chance to do their job
        }

        buffers forall (b ⇒ b.toString aka (b.toString + " at " + numThreads) must_== "bcdefghij") // this is the ideal we are looking for
        true
      }
      var nThreads = 100
      while (nThreads < 25000 && runThreads(nThreads)) {
        println("success with " + nThreads + " threads")
        nThreads *= 2
      }
      fail("Failed at " + nThreads)
    }

    "try the same with soft references" in {
      myTime = 0
//      sut.debugme = true
      val cached = sut.cache(() ⇒ { debug("sr: bump!"); counter = (counter+1).toChar; counter}).withSoftReferences.validFor(5).MINUTES
      counter = 'a'
      val numThreads: Int = 1//50//00
      val buffers = ((1 to numThreads) map ((i:Int) ⇒ new StringWriter)).toList
      val SEVEN_MINUTES = TimeUnit.MINUTES toNanos 7

      buffers foreach startConsuming(cached)

      Thread sleep 200

      for (i <- 1 to numTries) {
        myTime = myTime + SEVEN_MINUTES + 1000
        debug("will signal")
        signal
        Thread sleep 1000
      }
      buffers foreach (_.toString must_== "bcdefghij")
    }
  }
}*/