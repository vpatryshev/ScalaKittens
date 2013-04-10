package scalakittens

import org.specs.Specification

object ResultTest extends Specification {

  "Good" should {
    "be good" in {
      Good(42).isGood mustBe true
    }
    "be not bad" in {
      Good(43).isBad mustBe false
    }
    "list no errors" in {
      Good(math.Pi).listErrors.isEmpty mustBe true
    }
    "do nothing on error" in {
      var wasThere = false
      Good("I'm good").onError(errors => wasThere = true)
      wasThere aka "was called on error" must_== false
    }
    "return the contents on apply" in {
      Good("hi there")() must_== "hi there"
    }
    "Map as designed" in {
      Good("hello") map (_.toUpperCase) must_== Good("HELLO")
    }
    "flatMap as designed" in {
      Good("hello") flatMap (s => Good(s.toUpperCase)) must_== Good("HELLO")
      Good("hello") flatMap (s => Result.error(s, "alas...")) must_== Bad(ErrorDetails("alas...", "hello")::Nil)
      Good("hello") flatMap (s => NoResult) must_== NoResult
    }
    "collect as designed" in {
      val err = ErrorDetails(":(", ":)")
      Good("hello") collect ({ case "hello" => 1}, err) must_== Good(1)
      Good("hello") collect ({ case "Hello" => 1}, err) must_== Bad(err::Nil)
    }
    "convert to Some" in {
      Good(":)").toOption must_== Some(":)")
    }
    "stay put in orElse" in {
      Good(":)").orElse(NoResult) must_== Good(":)")
    }
    "stay put in getOrElse" in {
      Good(":)").getOrElse(":(") must_== ":)"
    }
    "blend properly via <*>" in {
      Good(1) <*> Good(2) must_== Good((1,2))
      Good(1) <*> Result.error(2, ":(") must_== Result.error(2, ":(")
      Good(1) <*> NoResult must_== NoResult
    }
    "call function in foreach" in {
      var v: String = ":("
      Good(":)") foreach (v = _)
      v must_== ":)"
    }
    "filter as designed" in {
      val err = ErrorDetails(":(", ":)")
      Good("hello") filter (_.startsWith("he"), "oi vei") must_== Good("hello")
      Good("hello") filter (_.startsWith("lo"), "oi vei") must_== Result.error("hello", "oi vei")
    }
    "Show nothing in errorDetails" in {
      Good("sh").errorDetails must_== None
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
      var ed: Traversable[ErrorDetails] = Nil
      Bad(errors).onError(es => { beenThere = true; ed=es})
      beenThere aka "visited what you were not supposed to visit" mustBe true
      ed must_== errors
    }
    "throw on apply" in {
      val errors = ErrorDetails("oi-vei", 740)::Nil
      val sut = Bad(errors)
      var beenThere = false
      try {
        sut()
        beenThere = true
      } catch {
        case ex: BadResultException => ex.errors must_== errors
        case _ => fail("Expected a Bad#ResultException")
      }
      beenThere mustBe false
    }
    "Map as designed" in {
      var wasThere = false
      Result.error(17, "oops") map (x => {
        wasThere = true; 1+x
      }) must_== Result.error(17, "oops")
      wasThere aka "visited what you were not supposed to visit" mustBe false
    }
    "flatMap as designed" in {
      var wasThere = false
      val r1 = Result.error("hello", "oops") flatMap (s => {wasThere = true; Good(s.toUpperCase)})
      r1 must_== Result.error("hello", "oops")
      wasThere aka "visited what you were not supposed to visit" mustBe false
      val r2 = Result.error("1 hello again", "oops") flatMap (s => {wasThere = true; Result.error(s, "alas...")})
      r2.isBad mustBe true
      r2.listErrors.toList must_== ErrorDetails("oops", "1 hello again")::Nil
      r2 must_== Bad(ErrorDetails("oops", "1 hello again")::Nil)
      wasThere aka "visited what you were not supposed to visit" mustBe false
      Result.error("2 and hello again", "oops") flatMap (s => {wasThere = true; NoResult}) must_== Bad(ErrorDetails("oops", "2 and hello again")::Nil)
      wasThere aka "visited what you were not supposed to visit" mustBe false
    }
    "collect nothing" in {
      var wasThere = false
      val err = ErrorDetails(":(", ":)")
      Result.error("hello", "oops") collect ({ case "hello" => {wasThere = true; 1}}, err) must_== Result.error("hello", "oops")
      wasThere aka "visited what you were not supposed to visit" mustBe false
      Result.error("hello", "oops")  collect ({ case "Hello" => {wasThere = true; 1}}, err) must_== Result.error("hello", "oops")
      wasThere aka "visited what you were not supposed to visit" mustBe false
    }
    "convert to None" in {
      Result.error("hello", "oops") .toOption must_== None
    }
    "get ignored in orElse" in {
      Result.error("hello", "oops") .orElse(Result.error("hello", ":(")) must_== Result.error("hello", ":(")
      Result.error("hello", "oops") .orElse(Good(":)")) must_== Good(":)")
      Result.error("hello", "oops") .orElse(NoResult) must_== NoResult
    }
    "get ignored in getOrElse" in {
      Result.error("hello", "oops").getOrElse(":)") must_== ":)"
    }
    "blend properly via <*>" in {
      Result.error(1, "oops") <*> Good(2) must_== Result.error(1, "oops")
      Result.error(1, "oops") <*> Result.error(2, ":(") must_== Bad(ErrorDetails("oops", 1)::ErrorDetails(":(", 2)::Nil)
      Result.error(1, "oops") <*> NoResult must_== Result.error(1, "oops")
    }
    "ignore call function in foreach" in {
      var wasThere = false
      Result.error("hello", "oops") foreach {s => wasThere = true}
      wasThere aka "visited what you were not supposed to visit" mustBe false
    }
    "filter as designed" in {
      val err = ErrorDetails(":(", ":)")
      Result.error("hello", "oops") filter (_.startsWith("he"), "oi vei") must_== Result.error("hello", "oops")
      Result.error("hello", "oops") filter (_.startsWith("lo"), "oi vei") must_== Result.error("hello", "oops")
    }
    "Merge errorDetails" in {
      val detailsOpt: Some[ErrorDetails] = Bad(ErrorDetails("beer too expensive") :: ErrorDetails("are we there?") :: Nil).errorDetails
      detailsOpt.isEmpty mustBe false
      val details = detailsOpt.get
      val desc = details.description
      val expectedDesc = "beer too expensive\nare we there?"
      desc must_== expectedDesc
      details must_== ErrorDetails(expectedDesc)
      detailsOpt must_== Some(ErrorDetails(expectedDesc))
    }
  }

  "NoResult" should {
    "be bad" in {
      NoResult.isGood mustBe false
    }
    "list errors" in {
      NoResult.listErrors.isEmpty mustBe true
    }
    "ignore on error" in {
      var beenThere = false
      NoResult.onError(es => { beenThere = true})
      beenThere mustBe false
    }
    "throw on apply" in {
      var beenThere = false
      val sut = NoResult
      var errorsCaught: Traversable[ErrorDetails] = Nil
      try {
        sut()
        beenThere = true
      } catch {
        case ex: BadResultException => {
          errorsCaught = ex.errors
        }
        case _ => fail("Expected a Bad#ResultException")
      }
      beenThere mustBe false
      errorsCaught must_== ErrorDetails("No results available")::Nil
    }
    "Map as designed" in {
      var wasThere = false
      NoResult map (x => {wasThere = true; null== x}) must_== NoResult
      wasThere aka "visited what you were not supposed to visit" mustBe false
    }
    "flatMap as designed" in {
      var wasThere = false
      NoResult flatMap (s => {wasThere = true; NoResult}) must_== NoResult
      wasThere aka "visited what you were not supposed to visit" mustBe false
    }
    "collect nothing" in {
      var wasThere = false
      val sut: Result[String] = NoResult
      sut collect ({ case "hello" => {wasThere = true; 1}}, null) must_== NoResult
      wasThere aka "visited what you were not supposed to visit" mustBe false
    }
    "convert to None" in {
      NoResult .toOption must_== None
    }
    "get ignored in orElse" in {
      NoResult .orElse(Result.error("hello", ":(")) must_== Result.error("hello", ":(")
      NoResult .orElse(Good(":)")) must_== Good(":)")
      NoResult .orElse(NoResult) must_== NoResult
    }
    "get ignored in getOrElse" in {
      NoResult.getOrElse(":)") must_== ":)"
    }

    "blend properly via <*>" in {
      NoResult <*> Good(2) must_== NoResult
      NoResult <*> Result.error(2, ":(") must_== NoResult
      NoResult <*> NoResult must_== NoResult
    }
    "ignore call function in foreach" in {
      var wasThere = false
      NoResult foreach ({x => wasThere = true})
      wasThere aka "visited what you were not supposed to visit" mustBe false
    }
    "filter as designed" in {
      val err = ErrorDetails(":(", ":)")
      NoResult filter (null !=, "oi vei") must_== NoResult
      NoResult filter (null ==, "oi vei") must_== NoResult
    }
    "Show 'missing' in errorDetails" in {
      NoResult.errorDetails must_== Some(ErrorDetails("No results"))
    }
  }

  "traverse" should {
    "return NoResult if the collection is empty" in {
      Result.traverse(Nil) must_== NoResult
    }
    "return all stuff in good case" in {
      Result.traverse(Good("abc")::Good("xyz")::Nil) must_== Good("abc"::"xyz"::Nil)
    }
    "return bads in bad case" in {
      Result.traverse(Result.error("abc", "bad1")::Good("xyz")::Result.error("123", "bad2")::NoResult::Nil) must_== Bad(ErrorDetails("bad1", "abc")::ErrorDetails("bad2", "123")::Nil)
    }
    "return NoResult if some results are missing, but no errors" in {
      Result.traverse(Good("abc")::Good("xyz")::NoResult:: Nil) must_== Result.error(List("abc", "xyz"), "Some results are missing")
    }
  }
}