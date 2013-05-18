package scalakittens

import org.specs2.mutable.Specification

object ResultTest extends Specification {

   "Good" should {
    "be good" in {
      Good(42).isGood must beTrue
    }
     "be not bad" in {
       Good(43).isBad must beFalse
     }
    "list no errors" in {
      Good(math.Pi).listErrors.isEmpty must beTrue
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
      Good("hello") flatMap (s => Result.error("alas...")) must_== Bad(("alas...")::Nil)
      Good("hello") flatMap (s => NoResult) must_== NoResult
    }
    "collect as designed" in {
      val err = (":(")
      Good("hello") collect ({ case "hello" => 1}, ":(") must_== Good(1)
      Good("hello") collect ({ case "Hello" => 1}, ":(") must_== Bad(err::Nil)
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
       Good(1) <*> Result.error(":(") must_== Result.error(":(")
       Good(1) <*> NoResult must_== NoResult
     }
     "call function in foreach" in {
       var v: String = ":("
       Good(":)") foreach (v = _)
       v must_== ":)"
     }
     "filter as designed" in {
       val err = (":(")
       Good("hello") filter ((s:String) => s.startsWith("he"), "oi vei") must_== Good("hello")
       Good("hello") filter ((s:String) => s.startsWith("lo"), "oi vei") must_== Result.error("oi vei")
     }
     "Show nothing in errorDetails" in {
       Good("sh").errorDetails must_== None
     }

     "combine with other goods in sugared loop" in {
       val actual = for (x <- Good("x");
                         y <- Good("y")) yield(x+y)
       actual must_== Good("xy")
     }
   }

  "Bad" should {
    "be bad" in {
      Bad(("what was the question?")::Nil).isGood must beFalse
    }
    "list errors" in {
      val errors = ("Whose life is it?")::
                   ("Hey Euler!"):: Nil
      Bad(errors).listErrors must_== errors
    }
    "behave on error" in {
      val errors = ("Say hi")::
                   ("Say bye"):: Nil
      var beenThere = false
      var ed: Traversable[String] = Nil
      Bad(errors).onError(es => { beenThere = true; ed=es})
      beenThere aka "visited what you were not supposed to visit" must beTrue
      ed must_== errors
    }
    "throw on apply" in {
      val errors = ("oi-vei")::Nil
      val sut = Bad(errors)
      var beenThere = false
      try {
        sut()
        beenThere = true
      } catch {
        case ex: BadResultException => ex.errors must_== errors
        case _: Throwable => failure("Expected a Bad#ResultException")
      }
      beenThere must beFalse
    }
    "Map as designed" in {
      var wasThere = false
      Result.error[Int]("oops") map ((x:Int) => {
        wasThere = true; 1+x
      }) must_== Result.error("oops")
      wasThere aka "visited what you were not supposed to visit" must beFalse
    }
    "flatMap as designed" in {
      var wasThere = false
      val r1 = Result.error[String]("oops") flatMap ((s:String) => {wasThere = true; Good(s.toUpperCase)})
      r1 must_== Result.error("oops")
      wasThere aka "visited what you were not supposed to visit" must beFalse
      val r20:Result[String] = Result.error("oops")
      val r2 = r20 flatMap (s => {wasThere = true; Result.error("alas...")})
      r2.isBad must beTrue
      r2.listErrors.toList must_== ("oops")::Nil
      r2 must_== Bad(("oops")::Nil)
      wasThere aka "visited what you were not supposed to visit" must beFalse
      val r3:Result[String] = Result.error("oops")
      r3 flatMap (s => {wasThere = true; NoResult}) must_== Bad(("oops")::Nil)
      wasThere aka "visited what you were not supposed to visit" must beFalse
    }
    "collect nothing" in {
      var wasThere = false
      val bad:Result[String] = Result.error("oops")
      bad collect ({ case "hello" => {wasThere = true; 1}}, ":(") must_== Result.error("oops")
      wasThere aka "visited what you were not supposed to visit" must beFalse
      val bad1:Result[String] = Result.error("oops")
      bad1 collect ({ case "Hello" => {wasThere = true; 1}}, ":(") must_== Result.error("oops")
      wasThere aka "visited what you were not supposed to visit" must beFalse
    }
    "convert to None" in {
      Result.error("oops") .toOption must_== None
    }
    "get ignored in orElse" in {
      Result.error("oops") .orElse(Result.error(":(")) must_== Result.error(":(")
      Result.error("oops") .orElse(Good(":)")) must_== Good(":)")
      Result.error("oops") .orElse(NoResult) must_== NoResult
    }
    "get ignored in getOrElse" in {
      Result.error("oops").getOrElse(":)") must_== ":)"
    }
    "blend properly via <*>" in {
      Result.error("oops") <*> Good(2) must_== Result.error("oops")
      Result.error("oops") <*> Result.error(":(") must_== Bad(("oops")::(":(")::Nil)
      Result.error("oops") <*> NoResult must_== Result.error("oops")
    }
    "ignore call function in foreach" in {
      var wasThere = false
      val bad: Result[String] = Result.error("oops")
      bad foreach {s => wasThere = true}
      wasThere aka "visited what you were not supposed to visit" must beFalse
    }
    "filter as designed" in {
      val err = (":(")
      Result.error[String]("oops") filter ((s:String) => s.startsWith("he"), "oi vei") must_== Result.error("oops")
      Result.error[String]("oops") filter ((s:String) => s.startsWith("lo"), "oi vei") must_== Result.error("oops")
    }
    "Merge errorDetails" in {
      val detailsOpt: Some[String] = Bad(("beer too expensive") :: ("are we there?") :: Nil).errorDetails
      detailsOpt.isEmpty must beFalse
      val desc = detailsOpt.get
      val expectedDesc = "beer too expensive; are we there?"
      desc must_== expectedDesc
      detailsOpt must_== Some((expectedDesc))
    }

    "combine with goods in sugared loop" in {
      val actual1 = for (x <- Bad[String](("x yourself")::Nil);
                         y <- Good("y")) yield(x+y)
      actual1 must_== Bad[String](("x yourself")::Nil)

      val actual2 = for (x <- Good("x");
                         y <- Bad[String](("y yourself")::Nil)) yield(x+y)
      actual2 must_== Bad[String](("y yourself")::Nil)
    }

    "combine with bads in sugared loop" in {
      val actual = for (x <- Bad[String](("x yourself")::Nil);
                        y <- Bad[String](("y yourself")::Nil)) yield(x+y)
      actual must_== Bad[String](("x yourself")::Nil)
    }

  }

  "NoResult" should {
    "be bad" in {
      NoResult.isGood must beFalse
    }
    "list errors" in {
      NoResult.listErrors.isEmpty must beTrue
    }
    "ignore on error" in {
      var beenThere = false
      NoResult.onError(es => { beenThere = true})
      beenThere must beFalse
    }
    "throw on apply" in {
      var beenThere = false
      val sut = NoResult
      var errorsCaught: Traversable[String] = Nil
      try {
        sut()
        beenThere = true
      } catch {
        case ex: BadResultException => {
          errorsCaught = ex.errors
        }
        case _:Throwable => failure("Expected a Bad#ResultException")
      }
      beenThere must beFalse
      errorsCaught must_== ("No results available")::Nil
    }
    "Map as designed" in {
      var wasThere = false
      NoResult map (x => {wasThere = true; null== x}) must_== NoResult
      wasThere aka "visited what you were not supposed to visit" must beFalse
    }
    "flatMap as designed" in {
      var wasThere = false
      NoResult flatMap (s => {wasThere = true; NoResult}) must_== NoResult
      wasThere aka "visited what you were not supposed to visit" must beFalse
    }
    "collect nothing" in {
      var wasThere = false
      val sut: Result[String] = NoResult
      sut collect ({ case "hello" => {wasThere = true; 1}}, null) must_== NoResult
      wasThere aka "visited what you were not supposed to visit" must beFalse
    }
    "convert to None" in {
      NoResult .toOption must_== None
    }
    "get ignored in orElse" in {
      NoResult .orElse(Result.error(":(")) must_== Result.error(":(")
      NoResult .orElse(Good(":)")) must_== Good(":)")
      NoResult .orElse(NoResult) must_== NoResult
    }
    "get ignored in getOrElse" in {
      NoResult.getOrElse(":)") must_== ":)"
    }

    "blend properly via <*>" in {
      NoResult <*> Good(2) must_== NoResult
      NoResult <*> Result.error(":(") must_== NoResult
      NoResult <*> NoResult must_== NoResult
    }
    "ignore call function in foreach" in {
      var wasThere = false
      NoResult foreach ({x => wasThere = true})
      wasThere aka "visited what you were not supposed to visit" must beFalse
    }
    "Filter as designed" in {
      val err = (":(")
      NoResult filter ((x:Any) => x != null, "oi vei") must_== NoResult
      NoResult filter ((x:Any) => x == null, "oi vei") must_== NoResult
    }
    "Show 'missing' in errorDetails" in {
      NoResult.errorDetails must_== Some(("No results"))
    }

    "combine with goods in sugared loop" in {
      val nr: Result[String] = NoResult
      val actual1 = for (x <- nr;
                         y <- Good("y")) yield(x+y)
      actual1 must_== NoResult

      val actual2 = for (x <- Good("x");
                         y <- nr) yield(x+y)
      actual2 must_== NoResult
    }

    "combine with bads in sugared loop" in {
      val nr: Result[String] = NoResult
      val actual1 = for (x <- nr;
                        y <- Bad[String](("y yourself")::Nil)) yield(x+y)
      actual1 must_== nr
      val actual2 = for (x <- Bad[String](("x yourself")::Nil);
                        y <- nr) yield(x+y)
      actual2 must_== Bad[String](("x yourself")::Nil)
    }

    "combine with NoResult in sugared loop" in {
      val nr: Result[String] = NoResult
      val actual1 = for (x <- nr;
                         y <- nr) yield(x+y)
      actual1 must_== nr
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
      Result.traverse(Result.error("abc")::Good("xyz")::Result.error("123")::NoResult::Nil) must_== Bad(("abc")::("123")::Nil)
    }
    "return NoResult if some results are missing, but no errors" in {
      Result.traverse(Good("abc")::Good("xyz")::NoResult:: Nil) must_== Result.error("Some results are missing")
    }
  }
}
