package scalakittens.experiments.logic

import org.specs2.mutable.Specification

/**
 * Created by vpatryshev on 9/26/17.
 */
class TernaryTest extends Specification {

  "TernaryTest" should {
    "¬" in {
      //      ¬ True mustBe False
      //
      //      ¬ Unknown mustBe False
      //      
      //      ¬ False mustBe True
      ok
    }

    "∨" in {
      (True ∨ True) mustEqual True
    }

    "∧" in {
      ok
    }

    "asOption" in {
      ok
    }

    "→" in {
      ok
    }

    "asBoolean" in {
      ok
    }

  }
}
