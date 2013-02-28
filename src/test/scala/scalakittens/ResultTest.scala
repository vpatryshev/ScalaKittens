package scalakittens

import org.specs.Specification

object ResultTest extends Specification {

  "Good" should {
    "be good" in {
      Good(42).isGood mustBe true
    }
    "list no errors" in {
      Good(math.Pi).listErrors.isEmpty mustBe true
    }
    "do nothing on error" in {
      Good("I'm good").onError(errors => fail("was supposed to ignore this"))
    }
    "return the contents on apply" in {
      Good("hi there")() must_== "hi there"
    }
  }

  "Bad" should {
    "be bad" in {
      Bad(ErrorDetails("what was the question?", 42)::Nil).isGood mustBe false
    }
    "list errors" in {
      val errors = ErrorDetails("Whose life is it?", math.Pi)::
        ErrorDetails("Hey Euler!", math.E):: Nil
      Bad(errors).listErrors must_== errors
    }
    "behave on error" in {
      val errors = ErrorDetails("Say hi", "hello")::
        ErrorDetails("Say bye", "chiao"):: Nil
      var beenThere = false
      Bad(errors).onError(es => { beenThere = true; es must_== errors})
      beenThere mustBe true
    }
    "throw on apply" in {
      val errors = ErrorDetails("oi-vei", 740)::Nil
      try {
        Bad(errors)
      } catch {
        case ex: BadResultException[String] => ex.errors must_== errors
        case _ => fail("Expected a Bad#ResultException")
      }
      Good("hi there")() must_== "hi there"
    }
  }
}
