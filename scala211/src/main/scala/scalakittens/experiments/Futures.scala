//package scalakittens.experiments

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
object Futures extends App {
  
  def fut(tag: String): Future[Unit] = Future[Unit] {
      for {
        n <- 0 to 1000
      } {
        println(tag + n)
        Thread.sleep(100)
      }
    }

  def m(): Int = {
    val f1 = fut("A")
    Thread.sleep(500)
    val f2 = fut("B")
    Thread.sleep(500)

    val comp = for {
      i <- f1
      j <- f2
    } yield (i, j)

    Await.result(comp, 5 seconds)
    
    42

  }
    
    
}
