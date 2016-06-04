package scalakittens

//import language.{implicitConversions, postfixOps, reflectiveCalls}
import java.util

import Result._
import org.specs2.mutable.Specification
import Ops._

class Ops_Test extends Specification {

   "tryOr()" should {
    "call the function once on success" in {
      var nCalls = 0
      var result = tryOr({nCalls += 1; "heureux"}, "malheureux")
      result must_== "heureux"
      nCalls must_== 1
    }

    "call the function once on failure" in {
      var nCalls = 0
      var result = tryOr({nCalls += 1; throw new IllegalArgumentException("wtf?!")}, "malheureux")
      result must_== "malheureux"
      nCalls must_== 1
    }

  }

  "asScala" should {
    "Convert an array into list" in {
      val sut = asScala(Array(Array("a","b","c"), Nil, Array("x","y","z")))
      sut must_== List(List("a","b","c"), Nil, List("x","y","z"))
    }

    "Convert an ArrayList into list" in {
      val l1 = new util.ArrayList[String]()
      l1.add("a"); l1.add("b"); l1.add("c")
      val l2 = new util.ArrayList[String]()
      val l3 = new util.ArrayList[String]()
      l3.add("x"); l3.add("y"); l3.add("z")
      val ll = new  util.ArrayList[util.ArrayList[String]]()
      ll.add(l1); ll.add(l2); ll.add(l3)
      val sut = asScala(ll)
      sut must_== List(List("a","b","c"), Nil, List("x","y","z"))
    }

    "Convert a java map to scala map" in {
      val source = new java.util.HashMap[String, java.util.Map[String, String]] {
        put("first", new java.util.HashMap[String, String]() {
          put("one", "1")
          put("two", "2")
        })
        put("second", new java.util.HashMap[String, String]() {
          put("ten", "10")
          put("twenty", "20")
        })

      }
      val sut = asScala(source)
      sut must_== Map("first" -> Map("one"->"1", "two"->"2"), "second"->Map("ten"->"10", "twenty"->"20"))
    }

    "Convert a set into a set" in {
      val sut = asScala(new java.util.HashSet[java.util.HashSet[String]](){
        add(new java.util.HashSet[String]() {
          add("red"); add("blue")
        })
        add(new java.util.HashSet[String]() {
          add("old"); add("nu")
        })
      })
      sut must_== Set(Set("red", "blue"), Set("old", "nu"))
    }
  }
  "OnTimer" should {
    "Delimit execution of stuff using timer" in {
      val startTime = System.currentTimeMillis()
      var lastChecked = startTime
      import scala.concurrent.duration._
      val ten: DurationInt = 300 // thank you specs2 for overriding Duration class
      val timeout = ten milliseconds
      val result = spendNotMoreThan(timeout) on {
        for (i <- 1 to 600) {
          lastChecked = System.currentTimeMillis()
          Thread.sleep(1)
        }
        Good(lastChecked - startTime)
      }
      result must_== Result.error(s"Timeout after $timeout")
    }
    "Delimit execution of stuff using timer, graciously" in {
      val startTime = System.currentTimeMillis()
      var lastChecked = startTime
      import scala.concurrent.duration._
      val ten: DurationInt = 300 // thank you specs2 for overriding Duration class
      val timeout = ten milliseconds
      val result = spendNotMoreThan(timeout) on {
        for (i <- 1 to 1000) {
          lastChecked = System.currentTimeMillis()
          Thread.sleep(1)
        }
        OK
      }

      result.isGood must beFalse
    }
    "Return the right stuff if calculated in due time" in {
      val startTime = System.currentTimeMillis()
      var lastChecked = startTime
      import scala.concurrent.duration._
      val ten: DurationInt = 1000 // thank you specs2 for overriding Duration class
      val timeout = ten milliseconds
      val result = spendNotMoreThan(timeout) on {
        for (i <- 1 to 5) {
          lastChecked = System.currentTimeMillis()
          Thread.sleep(1)
        }
        Good(":)")
      }
      result must_== Good(":)")
    }
  }

  "group by relationship" should {
    "not fail on empty" in {
      groupByRelationship[Int](_ < _)(Nil) must_== Nil
    }
    "work with singleton" in {
      groupByRelationship[Int](_ < _)(42::Nil) must_== List(List(42))
    }
    "be ok with duplicated entries" in {
      groupByRelationship[Int](_ < _)(42::42::Nil) must_== List(List(42),List(42))
      groupByRelationship[Int](_ <= _)(42::42::Nil) must_== List(List(42,42))
    }
    "work properly with a list of discrete chains" in {
      groupByRelationship[Int](_ < _)(List(3,2,1)) must_== List(List(3),List(2),List(1))
    }
    "work" in {
      val actual1 = groupByRelationship[Int](_ < _)(List(1,2,4,2))
      actual1 must_== List(List(1,2,4), List(2))
      val actual2 = groupByRelationship[Int](_ > _)(List(1,2,4,2))
      actual2 must_== List(List(1),List(2), List(4,2))
    }
  }
}
