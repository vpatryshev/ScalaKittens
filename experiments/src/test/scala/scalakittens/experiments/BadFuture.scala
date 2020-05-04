package scalakittens.experiments

import org.specs2.mutable.Specification

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
 * Experimenting with stupid approach to future
 */
class BadFuture extends Specification {

  object taktak

  "tak-tak" should {
    "behave" in {

      val cls = taktak.getClass
      val n = cls.getSimpleName
      println(s"Found name $n")
      ok
    }
  }

  "future in future" should {
    "misbehave" in {
      val log: ListBuffer[String] = new ListBuffer[String]()

      def internal: Future[Int] = Future {
        for {i <- 1 to 10} {
          Thread.sleep(1000)
          log.append(s"Internal attempt #$i")
        }
        42
      }

      def f2 = Future {
        log.append("Starting f2")
        val f1: Future[Int] = internal
        val x = Await.result(f1, 10 seconds)
        println(x)
        (x, 17)
      }

      try {
        val y = Await.result(f2, 5 seconds)
        println(y)
      } catch {
        case _: Throwable =>
        // whatever
      }
      println(log)
      ok
    }
  }
}
