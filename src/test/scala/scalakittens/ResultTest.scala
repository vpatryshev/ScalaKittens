package scalakittens

import org.specs.Specification

object ResultTest extends Specification {
  import Result._
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
      Good("hello") map (s => s.toUpperCase) must_== Good("HELLO")
    }
    "flatMap as designed" in {
      Good("hello") flatMap (s => Good(s.toUpperCase)) must_== Good("HELLO")
      Good("hello") flatMap (s => Result.error("alas...")) must_== Bad(("alas...")::Nil)
      Good("hello") flatMap (s => Empty) must_== Empty
    }
    "collect as designed" in {
      val err = (":(")
      Good("hello") collect ({ case "hello" => 1}, _ => ":(") must_== Good(1)
      Good("hello") collect ({ case "Hello" => 1}, _ => ":(") must_== Bad(err::Nil)
    }
    "convert to Some" in {
      Good(":)").toOption must_== Some(":)")
    }
    "stay put in orElse" in {
      Good(":)").orElse(Empty) must_== Good(":)")
    }
    "stay put in getOrElse" in {
      Good(":)").getOrElse(":(") must_== ":)"
    }
    "blend properly via <*>" in {
      Good(1) <*> Good(2) must_== Good((1,2))
      Good(1) <*> Result.error(":(") must_== Result.error(":(")
      Good(1) <*> Empty must_== Empty
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
        case _: Throwable => fail("Expected a Bad#ResultException")
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
      r3 flatMap (s => {wasThere = true; Empty}) must_== Bad(("oops")::Nil)
      wasThere aka "visited what you were not supposed to visit" must beFalse
    }
    "collect nothing" in {
      var wasThere = false
      val bad:Result[String] = Result.error("oops")
      bad collect ({ case "hello" => {wasThere = true; 1}}, _ => ":(") must_== Result.error("oops")
      wasThere aka "visited what you were not supposed to visit" must beFalse
      val bad1:Result[String] = Result.error("oops")
      bad1 collect ({ case "Hello" => {wasThere = true; 1}}, _ => ":(") must_== Result.error("oops")
      wasThere aka "visited what you were not supposed to visit" must beFalse
    }
    "convert to None" in {
      Result.error("oops") .toOption must_== None
    }
    "get ignored in orElse" in {
      Result.error("oops") .orElse(Result.error(":(")) must_== Result.error(":(")
      Result.error("oops") .orElse(Good(":)")) must_== Good(":)")
      Result.error("oops") .orElse(Empty) must_== Empty
    }
    "get ignored in getOrElse" in {
      Result.error("oops").getOrElse(":)") must_== ":)"
    }
    "blend properly via <*>" in {
      Result.error("oops") <*> Good(2) must_== Result.error("oops")
      Result.error("oops") <*> Result.error(":(") must_== Bad(("oops")::(":(")::Nil)
      Result.error("oops") <*> Empty must_== Result.error("oops")
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
      val actual = for (x <- Result.error[String]("x yourself");
                        y <- Result.error[String]("y yourself")) yield(x+y)
      actual must_== Bad[String](("x yourself")::Nil)
    }

    "work applicatively" in {
      val blended:Result[(Int,Int)] = Result.error[Int]("x yourself") <*> Result.error[Int]("y yourself")
      def sum(x:Int,y:Int):Int = x+y
      val actual:Result[Int] = blended map ((sum _).tupled)
      actual must_== Bad[Int]("x yourself"::"y yourself"::Nil)
    }
  }

  "Empty" should {
    "be bad" in {
      Empty.isGood must beFalse
    }
    "list errors" in {
      Empty.listErrors.isEmpty must beTrue
    }
    "ignore on error" in {
      var beenThere = false
      Empty.onError(es => { beenThere = true})
      beenThere must beFalse
    }
    "throw on apply" in {
      var beenThere = false
      val sut = Empty
      var errorsCaught: Traversable[String] = Nil
      try {
        sut()
        beenThere = true
      } catch {
        case ex: BadResultException => {
          errorsCaught = ex.errors
        }
        case _:Throwable => fail("Expected a Bad#ResultException")
      }
      beenThere must beFalse
      errorsCaught must_== ("No results available")::Nil
    }
    "map as designed" in {
      var wasThere = false
      Empty map (x => {wasThere = true; null== x}) must_== Empty
      wasThere aka "visited what you were not supposed to visit" must beFalse
    }
    "flatMap as designed" in {
      var wasThere = false
      Empty flatMap (s => {wasThere = true; Empty}) must_== Empty
      wasThere aka "visited what you were not supposed to visit" must beFalse
    }
    "collect nothing" in {
      var wasThere = false
      val sut: Result[String] = Empty
      sut collect ({ case "hello" => {wasThere = true; 1}}, _ => "whatevar") must_== Empty
      wasThere aka "visited what you were not supposed to visit" must beFalse
    }
    "convert to None" in {
      Empty .toOption must_== None
    }
    "get ignored in orElse" in {
      Empty .orElse(Result.error(":(")) must_== Result.error(":(")
      Empty .orElse(Good(":)")) must_== Good(":)")
      Empty .orElse(Empty) must_== Empty
    }
    "get ignored in getOrElse" in {
      Empty.getOrElse(":)") must_== ":)"
    }

    "blend properly via <*>" in {
      Empty <*> Good(2) must_== Empty
      Empty <*> Result.error(":(") must_== Empty
      Empty <*> Empty must_== Empty
    }
    "ignore call function in foreach" in {
      var wasThere = false
      Empty foreach ({x => wasThere = true})
      wasThere aka "visited what you were not supposed to visit" must beFalse
    }
    "Filter as designed" in {
      val err = (":(")
      Empty filter ((x:Any) => x != null, "oi vei") must_== Empty
      Empty filter ((x:Any) => x == null, "oi vei") must_== Empty
    }
    "Show 'missing' in errorDetails" in {
      Empty.errorDetails must_== Some(("No results"))
    }

    "combine with goods in sugared loop" in {
      val nr: Result[String] = Empty
      val actual1 = for (x <- nr;
                         y <- Good("y")) yield(x+y)
      actual1 must_== Empty

      val actual2 = for (x <- Good("x");
                         y <- nr) yield(x+y)
      actual2 must_== Empty
    }

    "combine with bads in sugared loop" in {
      val nr: Result[String] = Empty
      val actual1 = for (x <- nr;
                         y <- Bad[String](("y yourself")::Nil)) yield(x+y)
      actual1 must_== nr
      val actual2 = for (x <- Bad[String](("x yourself")::Nil);
                         y <- nr) yield(x+y)
      actual2 must_== Bad[String](("x yourself")::Nil)
    }

    "combine with Empty in sugared loop" in {
      val nr: Result[String] = Empty
      val actual1 = for (x <- nr;
                         y <- nr) yield(x+y)
      actual1 must_== nr
    }
  }

  "traverse" should {
    "return Empty if the collection is empty" in {
      Result.traverse(Nil) must_== Empty
    }
    "return all stuff in good case" in {
      Result.traverse(Good("abc")::Good("xyz")::Nil) must_== Good("abc"::"xyz"::Nil)
    }
    "return bads in bad case" in {
      Result.traverse(Result.error("abc")::Good("xyz")::Result.error("123")::Empty::Nil) must_== Bad(("abc")::("123")::Nil)
    }
    "return Empty if some results are missing, but no errors" in {
      Result.traverse(Good("abc")::Good("xyz")::Empty::Nil) must_== Good("abc"::"xyz"::Nil)
    }
  }
  "applicative functionality" should {
    "impress the public" in {
      implicit def app[X,Y](p: (X => Y, X)): Y = p._1(p._2)
      //     implicit def t21_to_t3[X,Y,Z](t:((X, Y), Z)):(X,Y,Z) = (t._1._1, t._1._2, t._2)
      implicit def app20[X1,X2,Z](t: ((X1 => X2 => Z, X1), X2)): Z = t._1._1(t._1._2)(t._2)
      implicit def app2[X1,X2,Z](t: (X1 => X2 => Z, X1, X2)): Z = t._1(t._2)(t._3)
      implicit def app3[X1,X2,X3,Z](t: (X1 => X2 => X3 => Z, X1, X2, X3)): Z = t._1(t._2)(t._3)(t._4)
      // etc; use ProductIterator for folding, instead of copy and paste
      val forty_two:String = (((n: Int) => n*7 + "!", 6))
      forty_two must_== "42!"
      val itWorks: String = ((n:Int) => (m:Int) => n*m+":)", 6, 7)
      itWorks must_== "42:)"

      val r0 = Result.forValue((n:Int) => (m:Int) => n*m + " :)")
      val r1 = Result.forValue(6)
      val r2 = Result.forValue(7)
      val r = r0 <*> r1 <*> r2
      val result:Result[String] = r map app20[Int, Int, String]
      result must_== Good("42 :)")
    }
  }

  "Accompanying object" should {
    "produce good from some" in {
      Result(Some("beer")) must_== Good("beer")
      Result(Some("beer"), "but not bud!") must_== Good("beer")
    }
    "produce Empty from None" in {
      Result(None) must_== Empty
    }
    "produce Bad from None" in {
      Result(None, "Bud") must_== Bad("Bud"::Nil)
    }
    "produce good from a value" in {
      Result.forValue("beer") must_== Good("beer")
      Result.forValue("beer", "but not bud!") must_== Good("beer")
    }
    "produce Empty from null" in {
      Result.forValue(null) must_== Empty
      Result.forValue(null, "bud") must_== Bad("bud"::Nil)
    }
    "produce Bad from exceptional values" in {
      Result.forValue(throw new UnsupportedOperationException("brain surgery")) must_== Bad("java.lang.UnsupportedOperationException: brain surgery"::Nil)
      Result.forValue(throw new UnsupportedOperationException("brain surgery"), "oi vei") must_== Bad("oi vei: java.lang.UnsupportedOperationException: brain surgery"::Nil)
    }
    "work properly with Either" in {
      Result(Left("life is bad")) must_== Result.error("life is bad")
      Result(Right(42)) must_== Good(42)
    }
    "work properly with two options" in {
      Result(None, None) must_== Empty
      Result(None, Some("que dolor, que dolor")) must_== Result.error("que dolor, que dolor")
      Result(Some(7688721L), None) must_== Good(7688721L)
      Result(Some(7688721L), Some("wrong number")) must_== Good(7688721L)
    }
    "work properly with two nullables" in {
      Result.goodOrBad[IllegalArgumentException](null, null) must_== Empty
      Result.goodOrBad(null, "que dolor, que dolor") must_== Result.error("que dolor, que dolor")
      Result.goodOrBad(7688721L, null) must_== Good(7688721L)
      Result.goodOrBad(7688721L, "wrong number") must_== Good(7688721L)
    }
    "work properly with an array of nullables" in {
      Result.goodOrBad(Array(null, null)) must_== Empty
      Result.goodOrBad(Array(null, "que dolor, que dolor")) must_== Result.error("que dolor, que dolor")
      Result.goodOrBad(Array("besame mucho", null)) must_== Good("besame mucho")
      Result.goodOrBad(Array("besame mucho", "porque te amo y me amas")) must_== Good("besame mucho")
      Result.goodOrBad(Array("el que tiene doble vida, la legal y escondida")) must_== Result.error("el que tiene doble vida, la legal y escondida")
      Result.goodOrBad(Array("y ahorre tu", "que me dices", "que me cuentes")) must_== Result.error("Wrong iterable List(y ahorre tu, que me dices, que me cuentes), need one or two elements")
    }
  }
}
