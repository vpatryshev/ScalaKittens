package scalakittens.experiments

import org.specs2.mutable.Specification

/**
  * Here we check whether java.lang.Class is still broken.
  * It's broken in Java 8.
  * 
  * Created by vpatryshev on 3/10/17.
  */
class Java8Failing extends Specification {
  
  object X { object Y { object Z }}

  private val xyz = X.Y.Z.getClass

  "java.lang.Class.getCanonicalName" should {
    "fail" in {
      // this one is the right solution
      val n1 = xyz.getName.split("\\.").last.split("\\$").filter(_.nonEmpty).mkString(".")
      n1 must_== "Java8Failing.X.Y.Z"

      def gcn = xyz.getCanonicalName
      gcn must throwA[InternalError]
    }
  }
  
  "java.lang.Class.getSimpleName" should {
    "fail" in {

      def gsn = xyz.getSimpleName
      gsn must throwA[InternalError]
    }
  }


}
