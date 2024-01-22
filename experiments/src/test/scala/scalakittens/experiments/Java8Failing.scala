package scalakittens.experiments

import org.specs2.mutable.Specification

/**
 * Here we check whether java.lang.Class is still broken.
 * It's broken in Java 8.
 *
 * Created by vpatryshev on 3/10/17.
 */
class Java8Failing extends Specification {

  object X {
    object Y {
      object Z

    }

  }

  private val xyz = X.Y.Z.getClass
  val version: String = System.getProperty("java.version")
  val v8 = "1.8.z"

  "java.lang.Class.getCanonicalName" should {
    "pass if done right" in {
      // this one is the right solution
      val n1 = xyz.getName.split("\\.").last.split("\\$").filter(_.nonEmpty).mkString(".")
      n1 must_== "Java8Failing.X.Y.Z"
    }
  }

  "java.lang.Class.getSimpleName" should {
    "fail on java8 if done straight" in {
      if (version < v8) {
        def gcn = xyz.getCanonicalName
        gcn must throwA[InternalError]
      }
      ok
    }
  }
}
