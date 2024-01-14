package scalakittens

import scala.io.Source
import scala.languageFeature.reflectiveCalls

import scalakittens.Result.OK
import scalakittens.testing.TestBase

class Props_Test extends TestBase
{
  import Library._

  "PropsOps.replaceAll" >> {
    "keep intact strings that don't match" >> {
      replaceAll(Map("abc" -> "<<<$1>>>"))("not an ABC") must_== "not an ABC"
    }
    "transform strings according to patterns" >> {
      replaceAll(Map(".*(abc).*" -> "<<<$1>>>"))("this is an abc!") must_== "<<<abc>>>"
    }
  }

  "Props.@@" >> {
    "not crash on four parameters" >> {
      val pf = props("a.b.c.d" -> "<<A B C D>>")
      val res = pf valueOf ("x", "b", "c", "d")
      OK mustBeGood
    }

    "pass a negative real-life case" >> {
      val key = "Private.InTheHouse.Bribe.sofar"
      val map = props("Private.In-The-House.Alcohol.Yearly" -> "$30.00",
                    "Private.Abroad.Bribe.Used" -> "$0.00",
                    "Private.In-The-House.Bribe.Used" -> "$15.00",
                    "Private.In-The-House.Bribe.Yearly" -> "$15.00",
                    "Private.In-The-House.Alcohol.Used" -> "$1734.48",
                    "Private.Abroad.Bribe.Yearly" -> "$15.00",
                    "Private.Abroad.Alcohol.Yearly" -> "$30.00",
                    "Private.Abroad.Alcohol.Used" -> "$0.00")
      val dictionary = Map("Somewhere" -> "WTF_is_it", "sofar" -> "Used", "max" -> "Yearly")
      val sut = map.translate(dictionary)
      val bad = sut valueOf key
      bad.isBad must beTrue
      bad.toString contains "Missing 'Private.InTheHouse.Bribe.sofar'" must beTrue
    }

    "pass a positive real-life case" >> {
      val key = "Private.InTheHouse.Bribe.sofar"
      val map = props("Private.In-The-House.Alcohol.Yearly" -> "$30.00",
        "Private.Abroad.Bribe.Used" -> "$0.00",
        "Private.In-The-House.Bribe.Used" -> "$15.00",
        "Private.In-The-House.Bribe.Yearly" -> "$15.00",
        "Private.In-The-House.Alcohol.Used" -> "$1734.48",
        "Private.Abroad.Bribe.Yearly" -> "$15.00",
        "Private.Abroad.Alcohol.Yearly" -> "$30.00",
        "Private.Abroad.Alcohol.Used" -> "$0.00")
      val dictionary = Map(
        "Somewhere" -> "WTF_is_it",
        "Alcohol" -> "Out_of_Pocket",
        "InTheHouse" -> "In-The-House",
        "Somewhere" -> "Abroad",
        "sofar" -> "Used", "max" -> "Yearly")
      val sut = map.translate(dictionary)
      sut valueOf key must_== Result.forValue("$15.00")
    }

    "pass a positive real-life case with transformer" >> {
      val transformer: String => String = s => s.toLowerCase.replaceAll("[^\\w\\.]", "")
      val map = props("Private.In-The-House.Alcohol.Yearly" -> "$30.00",
        "Private.Abroad.Bribe.Used" -> "$0.00",
        "Private.In-The-House.Bribe.Used" -> "$15.00",
        "Private.In-The-House.Bribe.Yearly" -> "$15.00",
        "Private.In-The-House.Alcohol.Used" -> "$1734.48",
        "Private.Abroad.Bribe.Yearly" -> "$15.00",
        "Private.Abroad.Alcohol.Yearly" -> "$30.00",
        "Private.Abroad.Alcohol.Used" -> "$0.00")(transformer)
      val dictionary = Map(
        "Bribe" -> "Bribe",
        "Private" -> "Private",
        "Somewhere" -> "Somewhere",
        "Alcohol" -> "Alcohol",
        "InTheHouse" -> "InTheHouse",
        "Somewhere" -> "abroad",
        "sofar" -> "used", "max" -> "yearly")
      val sut = map.translate(dictionary)
      sut valueOf ("Private", "InTheHouse", "Bribe", "sofar") aka sut.toString must_== Result.forValue("$15.00")
      sut valueOf ("Private", "Somewhere", "Alcohol", "max") aka sut.toString must_== Result.forValue("$30.00")
    }

    "pass a positive real-life case with regex" >> {
      val transformer: String => String = replaceAll("Used" -> "sofar", "Yearly" -> "max", "Out-of-(\\w+)" -> "Out$1", "In-The-(\\w+)" -> "InThe$1")
      val sut = props("Private.In-The-House.Alcohol.Yearly" -> "$30.00",
        "Private.Abroad.Bribe.Used" -> "$0.00",
        "Private.In-The-House.Bribe.Used" -> "$15.00",
        "Private.In-The-House.Bribe.Yearly" -> "$15.00",
        "Private.In-The-House.Alcohol.Used" -> "$1734.48",
        "Private.Abroad.Bribe.Yearly" -> "$15.00",
        "Private.Abroad.Alcohol.Yearly" -> "$30.00",
        "Private.Abroad.Alcohol.Used" -> "$0.00")(transformer)
      sut valueOf ("Private", "InTheHouse", "Bribe", "sofar") aka sut.toString must_== Result.forValue("$15.00")
      sut valueOf ("Private", "Abroad", "Alcohol", "max") aka sut.toString must_== Result.forValue("$30.00")
    }
  }

  "Props" should {
    "be able to add" in {
      val p1 = props("a" -> "1", "b" -> "2")
      val p2 = props("c" -> "3", "d" -> "4")
      val actual = p1 ++ p2
      actual must_== props("a" -> "1", "b" -> "2", "c" -> "3", "d" -> "4")
      props() ++ p1 must_== p1
      p1 ++ props() must_== p1

    }
    "addPrefix" >> {
      val fp: Props = props("a" -> "1", "b" -> "2")
      val sut = fp.addPrefix(":)")
      sut.isDefinedAt(":).a") must beTrue
      sut.isDefinedAt(":).b") must beTrue
      sut.isDefinedAt(":).c") must beFalse
      sut.isDefinedAt(":(.a") must beFalse
      sut(":).a") must_== "1"
      sut(":).b") must_== "2"
    }
    "++" >> {
      val fp1: Props = props("a" -> "1", "b" -> "2")
      val fp2: Props = props("b" -> "42", "c" -> "4")
      val sut = fp1 ++ fp2
      sut.isDefinedAt("a") must beTrue
      sut.isDefinedAt("b") must beTrue
      sut.isDefinedAt("c") must beTrue
      sut.isDefinedAt("d") must beFalse
      sut("a") must_== "1"
      sut("b") == "2" || sut("b") == "42" must  beTrue
      sut("c") must_== "4"
    }

    "accumulate" >> {
      val x = Props.accumulate(Nil)
      x.toString must_== "Empty()"
      val fp1: Props = props("a" -> "1", "b" -> "2")
      val fp2: Props = props("b" -> "42", "c" -> "4")
      val fp3: Props = props("c" -> "88", "d" -> "6")
      val sut = Props.accumulate(fp1::fp2::fp3::Nil)
      sut.isDefinedAt("a") must beTrue
      sut.isDefinedAt("b") must beTrue
      sut.isDefinedAt("c") must beTrue
      sut.isDefinedAt("d") must beTrue
      sut.isDefinedAt("e") must beFalse
      sut("a") must_== "1"
      sut("b") == "2" || sut("b") == "42" must  beTrue
      sut("c") == "4" || sut("c") == "88" must  beTrue
      sut("d") must_== "6"
    }

    "@@ behave on four parameters" >> {
      val pf = props("a.b.c.d" -> "<<A B C D>>")
      pf valueOf ("a", "b", "c", "d") must_== Good("<<A B C D>>")
      (pf valueOf ("x", "b", "c", "d")) mustBeBad
    }

    "work with regexes" >> {
      val pf = props("a1.b2.c3.d4" -> "<<A B C D>>")
      val actual: Result[String] = pf @@("a?", "b?", "c3", "d?")
      actual must_== Good("<<A B C D>>")
      (pf @@ ("x", "b", "c", "d")) mustBeBad
    }

    "reorder params" >> {
      val pf = props("a.b.c.d" -> "<<A B C D>>") ++ props("a1.b1.c1.d1" -> "<<A1 B1 C1 D1>>")
      val reordered = pf reorder(4,2,1,3)
      reordered valueOf ("c", "b", "d", "a") must_== Good("<<A B C D>>")
      reordered valueOf ("c1", "b1", "d1", "a1") must_== Good("<<A1 B1 C1 D1>>")
      (reordered valueOf ("a", "b", "c", "d")) mustBeBad
    }

    "reorder params, heterogeneous" >> {
      val pf = props("a.b.c.d" -> "<<A B C D>>")
      val sut = (pf reorder(4,2,1,3)) ++ props("clue" -> "treasure")
      val res = sut valueOf ("c", "b", "d", "a")
      res must_== Good("<<A B C D>>")
      (sut valueOf ("a", "b", "c", "d")).isBad must beTrue
      sut valueOf "clue" must_== Good("treasure")
    }

    "dropPrefix work on four parameters" >> {
      val pf = props("a.b.c.d" -> "<<ABCD>>", "x" -> "<<X>>")
      val dp = pf.dropPrefix
      dp @@ ("b", "c", "d") must_== Good("<<ABCD>>")
      (dp @@ "x").isBad must beTrue
    }

    "find having" >> {
      val sut = props("a.b.c.d" -> "<<ABCD>>", "x" -> "<<X>>", "a.d.x.y" -> "<<ADXY>>", "1.k" -> "K1", "k.48" -> "K1")
      sut.findHaving("c") must_== Good("<<ABCD>>")
      sut.findHaving("a").isBad must beTrue
      sut.findHaving("ab").isBad must beTrue
      sut.findHaving("y.d") must_== Good("<<ADXY>>")
      sut.findHaving("k") must_== Good("K1")
    }

    "find and replace" >> {
      val sut1 = props("a.b.c.d" -> "<<ABCD>>", "x" -> "<<X>>", "a.d.x.y" -> "<<ADXY>>")
      val sut2 = props("a.b.c.d" -> "<<ABCDZZ>>", "x" -> "<<X>>", "a.d.x.y" -> "<<ADXY>>")
      val replaced = sut1.findAndReplace("c", "<<ABCDZZ>>")
      replaced == sut2 must beTrue
      sut1.findAndReplace("ab", "oh really?!") must_== sut1
      val parser = Props.parse

      val sut3 = Props parse "fp(Map(\"Back to Transaction History.Account #2014151 Details.Health Net ID#\" -> \"R0000000-00\", \"Back to Transaction History.Account #20140101 Details.Name\" -> \"SCOTT GRISCHY\"))"
      sut3 match {
        case Good(props) =>
          val newOne = props.findAndReplace("Name", "Lia Van Damm")
          val newName = newOne.findHaving("Name")
          newName must_== Good("Lia Van Damm")
        case bad => failure(s"wrong text: $bad")
      }
      ok
    }

    "discard empty subtrees" >> {
      val sut = props("a.b.c.d" -> "<<ABCD>>", "x" -> "<<X>>", "a.d.x.y" -> "<<ADXY>>")
      sut.findAllHaving("d").toString must_== """fp(Map("a.b.c.d" -> "<<ABCD>>", "a.d.x.y" -> "<<ADXY>>"))"""
    }

    "extract one by subkey" >> {
      val sut = props("a.b.[[1]].c.d" -> "<<ABCD>>", "a.b.[[1]].c.y" -> "<<ABCY>>", "x" -> "<<X>>")
      sut.extractAllNumberedHaving("d").toList.toString must_== """List(fp(Map("c.d" -> "<<ABCD>>", "c.y" -> "<<ABCY>>")))"""
    }
    "trim prefixes wisely" >> {
      val p = props("a.b.c" -> "ABC")
      val t = p.trimPrefixes
      val s = t.toString
      s must_==props("c"->"ABC").toString
      props(  "a.b" -> "ABC").trimPrefixes.toString must_==props("b"->"ABC").toString
      props(    "a" -> "ABC").trimPrefixes.toString must_==props("a"->"ABC").toString
    }

    "pass a negative real-life case" >> {
      val key = "Private.InTheHouse.Bribe.sofar"
      val map = props("Private.In-The-House.Alcohol.Yearly" -> "$30.00",
        "Private.Abroad.Bribe.Used" -> "$0.00",
        "Private.In-The-House.Bribe.Used" -> "$15.00",
        "Private.In-The-House.Bribe.Yearly" -> "$15.00",
        "Private.In-The-House.Alcohol.Used" -> "$1734.48",
        "Private.Abroad.Bribe.Yearly" -> "$15.00",
        "Private.Abroad.Alcohol.Yearly" -> "$30.00",
        "Private.Abroad.Alcohol.Used" -> "$0.00")
      val dictionary = Map("Somewhere" -> "WTF_is_it", "Alcohol" -> "Out_of_Pocket", "sofar" -> "Used", "max" -> "Yearly")
      val sut = map.translate(dictionary)
      val bad = sut valueOf key
      bad.isBad must beTrue
      bad.toString contains "Missing 'Private.InTheHouse.Bribe.sofar'" must beTrue
    }

    "pass a positive real-life case" >> {
      val key = "Private.InTheHouse.Bribe.sofar"
      val map = props("Private.In-The-House.Alcohol.Yearly" -> "$30.00",
        "Private.Abroad.Bribe.Used" -> "$0.00",
        "Private.In-The-House.Bribe.Used" -> "$15.00",
        "Private.In-The-House.Bribe.Yearly" -> "$15.00",
        "Private.In-The-House.Alcohol.Used" -> "$1734.48",
        "Private.Abroad.Bribe.Yearly" -> "$15.00",
        "Private.Abroad.Alcohol.Yearly" -> "$30.00",
        "Private.Abroad.Alcohol.Used" -> "$0.00")
      val dictionary = Map(
        "Somewhere" -> "WTF_is_it",
        "Alcohol" -> "Out_of_Pocket",
        "InTheHouse" -> "In-The-House",
        "Somewhere" -> "Abroad",
        "sofar" -> "Used", "max" -> "Yearly")
      val sut = map.translate(dictionary)
      sut valueOf key must_== Result.forValue("$15.00")
    }

    "return a subtree" >> {
      val map = props("Private.In-The-House.Alcohol.Yearly" -> "$30.00",
        "Private.Abroad.Bribe.Used" -> "$0.00",
        "Private.In-The-House.Bribe.Used" -> "$15.00",
        "Private.In-The-House.Bribe.Yearly" -> "$15.00",
        "Private.In-The-House.Alcohol.Used" -> "$1734.48",
        "Private.Abroad.Bribe.Yearly" -> "$15.00",
        "Private.Abroad.Alcohol.Yearly" -> "$30.00",
        "House.Abroad.Alcohol.Used" -> "$0.01")
      val none = map.subtree("Nessuno")
      none.isEmpty must beTrue
      val ind = map.subtree("Private")
      ind.get("In-The-House.Alcohol.Used") must_== Some("$1734.48")
      ind.get("Abroad.Alcohol.Used") must_== None
      val fam = map.subtree("House")
      fam.get("Abroad.Alcohol.Used") must_== Some("$0.01")
      fam.get("In-The-House.Alcohol.Used") must_== None
      val deeper = map.subtree("Abroad")
      deeper.get("Bribe.Yearly") must_== Some("$15.00")
    }

    "return a set of keys" >> {
      val map = props("Private.In-The-House.Alcohol.Yearly" -> "$30.00",
        "Private.Abroad.Bribe.Used" -> "$0.00",
        "Private.In-The-House.Bribe.Used" -> "$15.00",
        "Private.In-The-House.Bribe.Yearly" -> "$15.00",
        "Private.In-The-House.Alcohol.Used" -> "$1734.48",
        "Private.Abroad.Bribe.Yearly" -> "$15.00",
        "Private.Abroad.Alcohol.Yearly" -> "$30.00",
        "House.Abroad.Alcohol.Used" -> "$0.01",
        "House.Out-of-the-head.Out-of-Sight.Gone" -> "nothing")
      map.keySet must_== Set("Private", "House")
      val none = map.subtree("Nessuno")
      none.keySet.isEmpty must beTrue
      val ind = map.subtree("Private")
      ind.keySet must_== Set("Abroad", "In-The-House")
      val fam = map.subtree("House")
      fam.keySet must_== Set("Abroad", "Out-of-the-head")
      val p = fam ++ ind
      p.keySet must_== Set("Abroad", "Out-of-the-head", "In-The-House")
    }

    "produce a subtree of ending with" >> {
      val sut = props("a.b.c" -> "<<ABC>>", "c" -> "<<C>>").endingWith("c")
      sut.toString must_== """fp(Map("a.b" -> "<<ABC>>"))"""
    }
  }

  "fromParallelLists" >> {
    "extract data when headers are the same" >> {
      val got = Props.fromParallelLists(List("A", "B", "C"), List("a1", "b1", "c1"), List("A", "B", "C"))
      got match {
        case Good(props) =>
          props @@ "A" must_== Good("a1")
          props @@ "B" must_== Good("b1")
          props @@ "C" must_== Good("c1")
        case bad => failure(s"Oops, $bad")
      }
      ok
    }
    "extract data when headers are messed up" >> {
      val got = Props.fromParallelLists(List("D", "B", "A", "C"), List("d1", "b1", "a1", "c1"), List("A", "B", "C"))
      got match {
        case Good(props) =>
          props @@ "A" must_== Good("a1")
          props @@ "B" must_== Good("b1")
          props @@ "C" must_== Good("c1")
        case bad => failure(s"Oops, $bad")
      }
      ok
    }

    "fail if headers are missing" >> {
      val got = Props.fromParallelLists(List("A", "C"), List("a1", "c1"), List("A", "B", "C"))
      got match {
        case bad: Bad[_] => bad.toString contains "(b)" must beTrue
        case shozanah => failure(s"Expected an error in $shozanah")
      }
      ok
    }
  }

  "From Map " >> {
    "drop indexes" >> {
      val sut0 = props("a.b.[[1]].c.d" -> "<<ABCD>>", "a.b.[[1]].c.y" -> "<<ABCY>>", "x" -> "<<X>>")
      val sut = sut0.dropIndexes
      sut @@ "a.b.c.d" must_== Good("<<ABCD>>")
      sut @@ "a.b.c.y" must_== Good("<<ABCY>>")
      sut @@ "x"       must_== Good("<<X>>")
    }
  }

  "transformKeys" >> {
    "work" >> {
      val sut0 = props("a" -> "1", "b" -> "2")
      val sut = sut0.transformKeys("<<"+_+">>")
      sut must_== props("<<a>>" -> "1", "<<b>>" -> "2")
    }
  }

  "Parser" >> {
    "parse simple sample as text" >> {
      val sutOpt = Props parse "fp(Map(\"a\" -> \"b\"))"
      sutOpt match {
        case Good(sut) =>
          sut @@ "a" must_== Good("b")
        case orelse => failure(s"Oops, not good: $orelse")
      }
      ok
    }

    "parse formatted too" >> {
      val text: String = """fp(
                             |Map(
                             |    "Description" -> "X-RAY EXAM",
                             |    "Patient Payment" -> "$31.84",
                             |    "Provider" -> "ALL PODIATRY GROUP",
                             |    "Provider Adjustment Amount" -> "$58.16",
                             |    "Provider Payment" -> "$0.00",
                             |    "Service Date" -> "03/26/2014",
                             |    "Total Submitted" -> "$90.00"
                             |))
                             | """.stripMargin
      val sutOpt = Props parse text
      sutOpt match {
        case Good(sut) =>
          sut @@ "Provider" must_== Good("ALL PODIATRY GROUP")
        case orelse => failure(s"Oops, not good: $orelse")
      }
      ok

    }

    "have left inverse" >> {
      val txt = props("a" -> "b").toString
      val sutOpt = Props parse txt
      sutOpt match {
        case Good(sut) =>
          sut @@ "a" must_== Good("b")
        case orelse => failure(s"Oops, not good: $orelse in $txt")
      }
      OK mustBeGood
    }

    "parse simple sample with dictionary as text" >> {
      val sutOpt = Props parse "fp(Map(\"a\" -> \"b\")) with dictionary Map(\"x\" -> \"y\", \"z\" -> \"a\")"
      sutOpt match {
        case Good(sut) =>
          sut @@ "z" must_== Good("b")
        case orelse => failure(s"Oops, not good: $orelse")
      }
      ok
    }

    "parse simple sample with reordering as text" >> {
      val sutOpt = Props parse "fp(Map(\"a.b.c.d\" -> \"cdba\")) with reordering (3,4,2,1)"
      sutOpt match {
        case Good(sut) =>
          sut @@ "d.c.a.b" must_== Good("cdba")
          case orelse => failure(s"Oops, not good: $orelse")
      }
      ok
    }

    "parse simple sample with dictionary and reordering as text" >> {
      val sutOpt = Props parse "fp(Map(\"a.b.c.d\" -> \"cdba\")) with dictionary Map(\"x\" -> \"y\", \"z\" -> \"a\") with reordering (3,4,2,1)"
      sutOpt match {
        case Good(sut) =>
          sut @@ "d.c.z.b" must_== Good("cdba")
        case orelse => failure(s"Oops, not good: $orelse")
      }
      ok
    }

    "parse OOP sample as text" >> {
      val sutOpt = Props parse "fp(Map(\"Bribe.Limit.Abroad.Protected\" -> \"$4,0.00\", \"Bribe.Accumulated.Home.Protected\" -> \"$0.00\", \"Bribe.How Much.Home.Protected\" -> \"$2,0.00\", \"Bribe.Limit.Home.Protected\" -> \"$2,0.00\", \"Bribe.Accumulated.Abroad.House\" -> \"$0.00\", \"Bribe.Accumulated.Home.House\" -> \"$249.00\", \"Bribe.Accumulated.Abroad.Protected\" -> \"$0.00\", \"Bribe.Limit.Home.House\" -> \"$4,0.00\", \"Bribe.How Much.Home.House\" -> \"$3,751.00\", \"Bribe.How Much.Abroad.Protected\" -> \"$4,0.00\", \"Bribe.Limit.Abroad.House\" -> \"$8,0.00\", \"Bribe.How Much.Abroad.House\" -> \"$8,0.00\")) " +
        "with dictionary Map(\"Private\" -> \"Protected\", \"max\" -> \"Limit\", \"Somewhere\" -> \"Abroad\", \"InTheHouse\" -> \"Home\", \"sofar\" -> \"Accumulated\", \"Alcohol\" -> \"Alcohol\") " +
        "with reordering (3,4,2,1)"
      sutOpt match {
        case Good(sut) =>
          sut @@ "Private.Somewhere.Bribe.max" must_== Good("$4,0.00")
        case orelse => failure(s"Oops, not good: $orelse")
      }
      ok
    }

    "parse suspicious Wednesday Addams data" >> {
      val text = "fp(Map(\"Bribe.Limit.Abroad.Protected\" -> \"$4,0.00\", \"Bribe.Accumulated.Home.Protected\" -> \"$0.00\", \"Bribe.How Much.Home.Protected\" -> \"$2,0.00\", \"Bribe.Limit.Home.Protected\" -> \"$2,0.00\", \"Bribe.Accumulated.Abroad.House\" -> \"$0.00\", \"Bribe.Accumulated.Home.House\" -> \"$249.00\", \"Bribe.Accumulated.Abroad.Protected\" -> \"$0.00\", \"Bribe.Limit.Home.House\" -> \"$4,0.00\", \"Bribe.How Much.Home.House\" -> \"$3,751.00\", \"Bribe.How Much.Abroad.Protected\" -> \"$4,0.00\", \"Bribe.Limit.Abroad.House\" -> \"$8,0.00\", \"Bribe.How Much.Abroad.House\" -> \"$8,0.00\")) with dictionary Map(\"Private\" -> \"Protected\", \"max\" -> \"Limit\", \"Somewhere\" -> \"Abroad\", \"InTheHouse\" -> \"Home\", \"sofar\" -> \"Accumulated\") with reordering (3,4,2,1)"
      val sutOpt = Props parse text
      sutOpt match {
        case Good(sut) =>
          sut @@ "Private.Somewhere.Bribe.max" must_== Good("$4,0.00")
        case orElse => failure(s"Oops, not good: $orElse")
      }
      ok
    }
  }

  "JSON" >> {
    val parseAndCheck = Function[String, Props]("json parser",
      (s: String) => {
        val propOpt = Props.parseJson(s)
        propOpt match {
          case Good(p) => p
          case bad => failure(s"Failed to parse <<$s>> got $bad"); ???
        }
      },
      (t: Throwable) => {t.printStackTrace(); throw t })

    val samples = List(
      "{}"  -> Props.empty,
      "{ \"v\":\"1\"}" -> props("v" -> "1"),
      "{ \"v\":\"ab'c\"}" -> props("v" -> "ab'c"),
      "{ \"v\":\"ab\\\"c\"}" -> props("v" -> "ab\"c"),
      "[ \"1\",\"2\",\"3\",\"4\"]" -> props("[[1]]" -> "1", "[[2]]" -> "2", "[[3]]" -> "3", "[[4]]" -> "4"),
      "{\"a\":\"hp://foo\"}" -> props("a" -> "hp://foo"),
      "{\"x\":[\"1.0\",\"2.0\",\"3.0\"]}" -> props("x.[[1]]" -> "1.0", "x.[[2]]" -> "2.0", "x.[[3]]" -> "3.0"),
      "[{\"a\":\"1.0\"},{\"b\":\"2.0\"},[\"3.0\",\"4.0\"]]" -> props("[[1]].a" -> "1.0", "[[2]].b" -> "2.0", "[[3]].[[1]]" -> "3.0", "[[3]].[[2]]" -> "4.0"),
      "{\"a\":{\"b\":\"c\"}}" -> props("a.b" -> "c"),
      "{\"c\":{\"p\":\"q\"}}" -> props("c.p" -> "q"),
      """{"affectedIDs":["11620"],"frameType":"2.0","date":"1405445271804","httpCode":"200","type":"Attachment","item":{"attachedOn":"1405445271","mimeType":"application/pdf","updatedAt":"1405445271","EOBID":"0","ID":"11620","userID":"10204","updatedMS":"1405445271","name":"statement.pdf","size":"790523"},"message":"OK"}""" -> props(
        "type" -> "Attachment",
        "date" -> "1405445271804",
        "frameType" -> "2.0",
        "message" -> "OK",
        "httpCode" -> "200",
        "affectedIDs.[[1]]" -> "11620",
        "item.ID" -> "11620",
        "item.EOBID" -> "0",
        "item.subsID" -> "",
        "item.uid" -> "",
        "item.userID" -> "10204",
        "item.name" -> "statement.pdf",
        "item.attachedOn" -> "1405445271", // pretty dumb eh
        "item.updatedAt" -> "1405445271",
        "item.mimeType" -> "application/pdf",
        "item.updatedMS" -> "1405445271",
        "item.size" -> "790523"
    ))

    "check quotes" >> {
      parseAndCheck maps ("{ \"v\":\"ab\\\"c\"}" -> props("v" -> "ab\"c"))
      ok
    }

    "parse" >> {
      val extraSamples = List(
        //bad json lib        "{\"v\":\"1\"\\n}"  -> props("v" -> "1"),
        //bad json lib        "{\"v\":\"1\"\\r\\n}"  -> props("v" -> "1"),
        //bad json lib        "{\t\"v\":\"1\"\\n}"  -> props("v" -> "1"),
        //bad json lib        "{\t\"v\":\"1\"\\r\\n}"  -> props("v" -> "1"),
        //bad json lib        "{\"v\":123456789123456789123456789}" -> props("v" -> "123456789123456789123456789"),
        "{ \"v\":1}" -> props("v" -> "1.0"), // why?!
        "[ 1,2,3,4]" -> props("[[1]]" -> "1.0", "[[2]]" -> "2.0", "[[3]]" -> "3.0", "[[4]]" -> "4.0"),
        "[ \"1\",\"2\",\"3\",\"4\"]" -> props("[[1]]" -> "1", "[[2]]" -> "2", "[[3]]" -> "3", "[[4]]" -> "4"),
        "[ { }, { },[]]" -> Props.empty,
        "{\"a\":null}" -> Props.empty,
        "{\"a\":true}" -> props("a" -> "true"),
        "{\"x\":[1,2,3]}" -> props("x.[[1]]" -> "1.0", "x.[[2]]" -> "2.0", "x.[[3]]" -> "3.0"),
        "[{\"a\":1},{\"b\":2},[3,4]]" -> props("[[1]].a" -> "1.0", "[[2]].b" -> "2.0", "[[3]].[[1]]" -> "3.0", "[[3]].[[2]]" -> "4.0"),
        "{\"a\":{\"b\":\"c\"}}" -> props("a.b" -> "c"),
        "{\"a\":[],\"b\":{},\"c\":{\"p\":\"q\"}}" -> props("c.p" -> "q"),
        """{
          "type" : "Attachment",
          "date" : 1405445271804,
          "frameType" : 2,
          "message" : "OK",
          "httpCode" : 200,
          "affectedIDs" : [
                  "11620"
          ],
          "item" : {
                  "ID" : 11620,
                  "EOBID" : 0,
                  "subsID" : "",
                  "uid" : "",
                  "userID" : 10204,
                  "name" : "statement.pdf",
                  "attachedOn" : 1405445271000,
                  "mimeType" : "application\\/pdf",
                  "updatedAt" : 1405445271000,
                  "updatedMS" : 1405445271000,
                  "size" : 790523
          }
          }""" -> props(
          "type" -> "Attachment",
          "date" -> "1.405445271804E12",
          "frameType" -> "2.0",
          "message" -> "OK",
          "httpCode" -> "200.0",
          "affectedIDs.[[1]]" -> "11620",
          "item.ID" -> "11620.0",
          "item.EOBID" -> "0.0",
          "item.subsID" -> "",
          "item.uid" -> "",
          "item.userID" -> "10204.0",
          "item.name" -> "statement.pdf",
          "item.attachedOn" -> "1.405445271E12", // pretty dumb eh
          "item.updatedAt" -> "1.405445271E12",
          "item.mimeType" -> "application\\/pdf",
          "item.updatedMS" -> "1.405445271E12",
          "item.size" -> "790523.0"
        ))

      parseAndCheck maps ((samples++extraSamples).toArray:_*)

      Props.parseJson("{'X':'s").isBad must beTrue
      Props.parseJson("{'X").isBad must beTrue
    }

    "generate" >> {
      props("[[1]]" -> "1", "[[2]]" -> "2", "[[3]]" -> "3", "[[4]]" -> "4").toJsonString must_== """["1", "2", "3", "4"]"""

      val generate = Function[Props, String]("json stringifier",
        (p:Props) => {
          val s = p.toJsonString
          s.replaceAll("[ \\n]", "")
        },
        (t:Throwable) => {t.printStackTrace(); throw t})
        generate maps (samples.map { case (j, p) => (p, j.replaceAll(" ", ""))}.toArray: _*)
    }
  }

  "all these functions" >> {
    "Keep props intact in groupByIndex if there are no indexes" >> {
      val pp = props(Map("key1" -> "val1", "key2" -> "val2"))
      val sut = pp.groupByIndex
      sut.size must_== 1
      sut(0) must_== pp
    }

    "Group by index in simple case" >> {
      val pp = props(Map(
        "[[1]].key01" -> "val01", "[[1]].key02" -> "val02",
        "[[2]].key11" -> "val11", "[[2]].key12" -> "val12",
        "[[3]].key21" -> "val21", "[[3]].key22" -> "val22"
      ))
      val sut = pp.groupByIndex
      sut.size must_== 3
      for (i <- 0 to 2;
           j <- 1 to 2) {
        sut(i)(s"key$i$j") must_== s"val$i$j"
      }
      ok
    }

    "Group by index in double index case" >> {
      val pp = props(Map(
        "[[1]].[[1]].key01" -> "val01", "[[1]].[[1]].key02" -> "val02",
        "[[2]].[[1]].key11" -> "val11", "[[2]].[[1]].key12" -> "val12",
        "[[3]].[[1]].key21" -> "val21", "[[3]].[[1]].key22" -> "val22"
      ))
      val sut = pp.groupByIndex.toList
      sut.size must_== 3
      for (i <- 0 to 2;
           j <- 1 to 2) {
        sut(i)(s"[[1]].key$i$j") must_== s"val$i$j"
      }
      ok
    }

    "Behave in a modified regression case of embedded array" >> {

      val text05062014="fp(Map(\"[[1]].[[1]].K\" -> \"I0\", \"[[3]].[[1]].K\" -> \"I2\", \"[[2]].[[1]].K\" -> \"I1\"))"
      val props05062014 = Props parse text05062014

      props05062014 match {
        case Good(props) =>
          val sut = props.groupByIndex
          //          val sut = props.gbi2(1 to 3)
          //          sut.toString.contains ("I0") must beTrue
          sut.size must_== 3
          val serviceTypes = sut map (_.getOrElse("[[1]].K", "oops..."))
          serviceTypes.toList must_== List("I0", "I1", "I2")
        case basura => failure(s"Oops, not good: $basura")
      }
      OK mustBeGood
    }

    "parse when keys contain commas" >> {
      val text = "fp(Map(\"DoS\" -> \"01/29/2014\", \"HERMIONE GRANGER, MUGGLE.Passport Number\" -> \"9\"))"
      val propsOpt = Props parse text
      propsOpt match {
        case Good(props) =>
          props @@ "HERMIONE GRANGER, MUGGLE.Passport Number" must_== Good("9")
        case basura => failure(s"Oops, not good: $basura")
      }
      ok
    }

    "Group by index using real data" >> {
      val text = "fp(Map(\"[[1]].[[1]].Fuel Burned\" -> \"$26.80\", \"[[1]].[[2]].Distance Limit\" -> \"$5.00\", \"[[4]].[[1]].Distance Flown\" -> \"$0.00\", \"[[3]].[[2]].Wasted\" -> \"$12.00\", \"[[1]].[[2]].Wasted\" -> \"$5.00\", \"[[4]].[[1]].Departure Date\" -> \"03/18/2014 - 03/18/2014\", \"[[2]].[[2]].Wasted\" -> \"$26.00\", \"[[2]].[[1]].Purpose of the Trip\" -> \"Pure Entertainment\", \"[[5]].[[2]].Wasted\" -> \"$9.00\", \"[[3]].[[1]].Departure Date\" -> \"03/18/2014 - 03/18/2014\", \"[[4]].[[1]].Fuel Burned\" -> \"$49.30\", \"[[5]].[[1]].Purpose of the Trip\" -> \"Dirty Entertainment\", \"[[5]].[[1]].Flyweight\" -> \"$9.00\", \"[[2]].[[2]].Tax\" -> \"$0.00\", \"[[4]].[[1]].Purpose of the Trip\" -> \"Mixed Entertainment\", \"[[3]].[[1]].Distance Flown\" -> \"$0.00\", \"[[3]].[[2]].Tip\" -> \"$0.00\", \"[[2]].[[2]].Distance Limit\" -> \"$26.00\", \"[[4]].[[2]].Distance Limit\" -> \"$7.00\", \"[[4]].[[2]].Tax\" -> \"$0.00\", \"[[5]].[[1]].Distance Flown\" -> \"$0.00\", \"[[4]].[[2]].Wasted\" -> \"$7.00\", \"[[3]].[[1]].Purpose of the Trip\" -> \"Sinful Fun\", \"[[2]].[[2]].Tip\" -> \"$0.00\", \"[[1]].[[2]].Tip\" -> \"$0.00\", \"[[1]].[[2]].Tax\" -> \"$0.00\", \"[[2]].[[1]].Departure Date\" -> \"03/18/2014 - 03/18/2014\", \"[[3]].[[1]].Futeemel Burned\" -> \"$87.60\", \"[[4]].[[2]].Tip\" -> \"$0.00\", \"[[3]].[[2]].Tax\" -> \"$0.00\", \"[[5]].[[2]].Tax\" -> \"$0.00\", \"[[5]].[[2]].Tip\" -> \"$0.00\", \"[[1]].[[1]].Purpose of the Trip\" -> \"36415 - Surgery\", \"[[5]].[[1]].Departure Date\" -> \"03/18/2014 - 03/18/2014\", \"[[5]].[[1]].Fuel Burned\" -> \"$40.50\", \"[[2]].[[1]].Flyweight\" -> \"$26.00\", \"[[3]].[[1]].Flyweight\" -> \"$12.00\", \"[[2]].[[1]].Fuel Burned\" -> \"$159.30\", \"[[1]].[[1]].Flyweight\" -> \"$5.00\", \"[[4]].[[1]].Flyweight\" -> \"$7.00\", \"[[5]].[[2]].Distance Limit\" -> \"$9.00\", \"[[1]].[[1]].Departure Date\" -> \"03/18/2014 - 03/18/2014\", \"[[2]].[[1]].Distance Flown\" -> \"$0.00\", \"[[3]].[[2]].Distance Limit\" -> \"$12.00\", \"[[1]].[[1]].Distance Flown\" -> \"$0.00\"))"
      val propsOpt = Props parse text
      propsOpt match {
        case Good(props) =>
          val sut = props.groupByIndex.zipWithIndex
          sut.size must_== 5
          val empty = sut.find(_._1.isEmpty)
          empty.isEmpty aka s"#{empty._2}" must beTrue
        case basura => failure(s"Oops, not good: $basura")
      }
      ok
    }

    "Group by index in transitional" >> {
      val text="fp(Map(\"[[2]].[[1]].Tip\" -> \"$0.00\", \"[[3]].[[1]].Distance Limit\" -> \"$27.13\", \"[[1]].[[1]].Distance Flown by your Best Friend\" -> \"$0.00\", \"[[3]].[[1]].Departure Date\" -> \"10/16/2013 - 10/16/2013\", \"[[3]].[[1]].Distance Flown by your Best Friend\" -> \"$0.00\", \"[[1]].[[1]].key01\" -> \"val01\", \"[[2]].[[1]].Distance Limit\" -> \"$16.00\", \"[[3]].[[1]].Wasted\" -> \"$0.00\", \"[[2]].[[1]].Departure Date\" -> \"10/16/2013 - 10/16/2013\", \"[[3]].[[1]].Fuel Burned\" -> \"$38.00\", \"[[1]].[[1]].Wasted\" -> \"$91.49\", \"[[2]].[[1]].Wasted\" -> \"$0.00\", \"[[3]].[[1]].key21\" -> \"val21\", \"[[2]].[[1]].Distance Flown by your Best Friend\" -> \"$0.00\", \"[[1]].[[1]].Tax\" -> \"$0.00\", \"[[2]].[[1]].key11\" -> \"val11\", \"[[2]].[[1]].Tax\" -> \"$0.00\", \"[[1]].[[1]].Distance Flown by your Traditional Partner\" -> \"$0.00\", \"[[3]].[[1]].Distance Flown by your Traditional Partner\" -> \"$27.13\", \"[[1]].[[1]].Departure Date\" -> \"10/16/2013 - 10/16/2013\", \"[[2]].[[1]].Fuel Burned\" -> \"$30.00\", \"[[3]].[[1]].Tip\" -> \"$0.00\", \"[[1]].[[1]].Distance Limit\" -> \"$91.49\", \"[[2]].[[1]].Distance Flown by your Traditional Partner\" -> \"$16.00\", \"[[1]].[[1]].Flyweight\" -> \"$91.49\", \"[[2]].[[1]].Flyweight\" -> \"$0.00\", \"[[1]].[[1]].Fuel Burned\" -> \"$144.00\", \"[[3]].[[1]].Flyweight\" -> \"$0.00\", \"[[3]].[[1]].Tax\" -> \"$0.00\", \"[[1]].[[1]].Tip\" -> \"$0.00\"))"
      val propsOpt = Props parse text
      propsOpt match {
        case Good(props) =>
          val sut = props.groupByIndex
          sut.size must_== 3
          for (i <- 0 to 2;
               j <- 1 to 1) {
            sut(i)(s"[[1]].key$i$j") must_== s"val$i$j"
          }
          val serviceTypes = sut map (_.getOrElse("[[1]].key", "oops..."))
        case basura => failure(s"Oops, not good: $basura")
      }
      ok
    }

    "Behave in a regression of 10/16/2013" >> {
      val text="fp(Map(\"[[2]].[[1]].Tip\" -> \"$0.00\", \"[[3]].[[1]].Distance Limit\" -> \"$27.13\", \"[[1]].[[1]].Distance Flown by your Best Friend\" -> \"$0.00\", \"[[3]].[[1]].Departure Date\" -> \"10/16/2013 - 10/16/2013\", \"[[3]].[[1]].Distance Flown by your Best Friend\" -> \"$0.00\", \"[[1]].[[1]].Purpose of the Trip\" -> \"Private Beach Party\", \"[[2]].[[1]].Distance Limit\" -> \"$16.00\", \"[[3]].[[1]].Wasted\" -> \"$0.00\", \"[[2]].[[1]].Departure Date\" -> \"10/16/2013 - 10/16/2013\", \"[[3]].[[1]].Official Distance\" -> \"$38.00\", \"[[1]].[[1]].Wasted\" -> \"$91.49\", \"[[2]].[[1]].Wasted\" -> \"$0.00\", \"[[3]].[[1]].Purpose of the Trip\" -> \"Private Show\", \"[[2]].[[1]].Distance Flown by your Best Friend\" -> \"$0.00\", \"[[1]].[[1]].Tax\" -> \"$0.00\", \"[[2]].[[1]].Purpose of the Trip\" -> \"Thai Massage\", \"[[2]].[[1]].Tax\" -> \"$0.00\", \"[[1]].[[1]].Distance Flown by your Traditional Partner\" -> \"$0.00\", \"[[3]].[[1]].Distance Flown by your Traditional Partner\" -> \"$27.13\", \"[[1]].[[1]].Departure Date\" -> \"10/16/2013 - 10/16/2013\", \"[[2]].[[1]].Fuel Burned\" -> \"$30.00\", \"[[3]].[[1]].Tip\" -> \"$0.00\", \"[[1]].[[1]].Distance Limit\" -> \"$91.49\", \"[[2]].[[1]].Distance Flown by your Traditional Partner\" -> \"$16.00\", \"[[1]].[[1]].Flyweight\" -> \"$91.49\", \"[[2]].[[1]].Flyweight\" -> \"$0.00\", \"[[1]].[[1]].Fuel Burned\" -> \"$144.00\", \"[[3]].[[1]].Flyweight\" -> \"$0.00\", \"[[3]].[[1]].Tax\" -> \"$0.00\", \"[[1]].[[1]].Tip\" -> \"$0.00\"))"
      val propsOpt = Props parse text
      propsOpt match {
        case Good(props) =>
          val sut = props.groupByIndex
          sut.size must_== 3
          val serviceTypes = sut map (_.getOrElse("[[1]].Purpose of the Trip", "oops..."))
          serviceTypes.toList must_== List("Private Beach Party", "Thai Massage", "Private Show")
        case basura => failure(s"Oops, not good: $basura")
      }
      ok
    }

    "regression 1 of 07/03/2014" >> {
      val text = "fp(Map(\"DoS\" -> \"01/29/2014\", \"HERMIONE GRANGER (MUGGLE).Passport Number\" -> \"9\"))"
      val propsOpt = Props parse text
      propsOpt match {
        case Good(props) =>
          props @@ ".Passport Number" must_== Good("9")
        case basura => failure(s"Oops, not good: $basura")
      }
      ok
    }
    "Extract data from list" >> {
      fromTree("first"::"second"::Nil) must_== Good(props("[[1]]"->"first", "[[2]]"->"second"))
    }

    "parse pair with a list as value" >> {
      isPrimitive("first"::"second"::Nil) must beFalse
      val actual = parsePair("key", "first"::"second"::Nil)
      actual must_== Good(props("key.[[1]]"->"first", "key.[[2]]"->"second"))
    }

    "Extract data from list in map" >> {
      val actual = fromTree(Map("key" -> ("first"::"second"::Nil)))
      actual must_== Good(props("key.[[1]]"->"first", "key.[[2]]"->"second"))
    }

    "Extract data from full tree" >> {
      val source = Map(
        "abroad" -> Map(
          "hasFamilyHome" -> false,
          "individualGoalsMet" -> List(0),
          "hasForeignAssets" -> true,
          "familyHome" -> -1,
          "hasIndividualGoals" -> true,
          "fname" -> List("JANE"),
          "individualGoals" -> 14000,
          "familyGoals" -> -1,
          "typeTitle" -> "Individual",
          "individualHomeMet" -> List(0),
          "bitcoins" -> 20,
          "hasFamilyGoals" -> false,
          "individualHome" -> 7000,
          "hasIndividualHome" -> true,
          "familyHomeMet" -> -1,
          "familyGoalsMet" -> -1),
        "isHSA" -> false,
        "isSubscriber" -> true,
        "isIndividual" -> true,
        "inNetwork" -> Map(
          "pharmHomeGeneric" -> -1, "hasFamilyHome" -> false,
          "individualGoalsMet" -> List(0), "familyHome" -> -1,
          "hasDrugHomeBrand" -> false, "hasDrugHome" -> false,
          "hasIndividualGoals" -> true, "pharmHome" -> -1,
          "fname" -> List("JANE"), "individualGoals" -> 3500,
          "hasDrugHomeGeneric" -> false, "hasInNetworkAssets" -> true,
          "familyGoals" -> -1, "typeTitle" -> "Individual",
          "individualHomeMet" -> List(0), "pharmHomeBrand" -> -1,
          "bitcoins" -> 0, "hasFamilyGoals" -> false,
          "individualHome" -> 3500, "hasIndividualHome" -> true,
          "familyHomeMet" -> -1, "familyGoalsMet" -> -1))

      val pp = fromTree(source)
      pp match {
        case Good(p) =>
          p @@ "inNetwork.individualGoalsMet.[[1]]" aka p.toString must_== Good("0")
          p @@ "abroad.individualHome" aka p.toString  must_== Good("7000")
        case oops => failure(s"oops: $oops")
      }
      ok
    }

    "Group Indices from a unordered set of indices" >> {
      val source = props(
        "[[1]].name" -> "ABC",
        "[[1]].foo" -> "abcbar",
        "[[3]].name" -> "DEF",
        "[[3]].foo" -> "defbar",
        "[[5]].name" -> "GHI",
        "[[5]].bar" -> "ghifoo"
      )
      val groupedProps = source.groupByIndex
      groupedProps.size must_== 5
      groupedProps.count(_.isEmpty) must_== 2
      groupedProps(4) @@ "bar" map (_=="ghifoo") must_== Good(true)

    }

    "Divyesh's regression" in {
      val mergedDataMaps = Set(Map.empty[String, String]::Map.empty[String, String]::Nil)

      val listOfMaps: List[Map[String, String]] = mergedDataMaps.toList.flatten
      val m:Map[String, String] = listOfMaps reduce (_++_)

      val p = Props(m)
      p.isEmpty must beTrue
    }

    "From input" in {
      val src =
        """
          |# Build Information #
          |BUILD_TIME   = 1451792555970
          |BUILD_NUMBER = ?
          |BUILD_ID     = ?
          |HOSTNAME     = "Craigs-MacBook-Pro-2.local"
          |GITINFO      = "commit 03deea7f25e88b6a89d12e117e96082ccbc27830; Author: Vlad Patryshev <vpatryshev@gmail.com>; Date:   Wed Dec 30 21:18:34 2015 -0800;"
          |BRANCH       = "develop"
        """.stripMargin
      val actual = fromSource(Source.fromString(src))
      actual must_== props(
        "BUILD_TIME" -> "1451792555970",
        "BUILD_NUMBER" -> "?",
        "BUILD_ID" -> "?",
        "HOSTNAME" -> "Craigs-MacBook-Pro-2.local",
        "GITINFO" -> "commit 03deea7f25e88b6a89d12e117e96082ccbc27830; Author: Vlad Patryshev <vpatryshev@gmail.com>; Date:   Wed Dec 30 21:18:34 2015 -0800;",
        "BRANCH" -> "develop"
      )
    }

    "From lines" in {
      val src =
        "# Build Information #"::
        "BUILD_TIME   = 1451792555970"::
          "BUILD_NUMBER = ?"::
          "BUILD_ID     = ?"::
          "HOSTNAME     = \"Craigs-MacBook-Pro-2.local\""::
          "GITINFO      = \"commit 03deea7f25e88b6a89d12e117e96082ccbc27830; Author: Vlad Patryshev <vpatryshev@gmail.com>; Date:   Wed Dec 30 21:18:34 2015 -0800;\""::
          "BRANCH       = \"develop\""::Nil
      val actual = fromPropLines(src)
      actual must_== props(
        "BUILD_TIME" -> "1451792555970",
        "BUILD_NUMBER" -> "?",
        "BUILD_ID" -> "?",
        "HOSTNAME" -> "Craigs-MacBook-Pro-2.local",
        "GITINFO" -> "commit 03deea7f25e88b6a89d12e117e96082ccbc27830; Author: Vlad Patryshev <vpatryshev@gmail.com>; Date:   Wed Dec 30 21:18:34 2015 -0800;",
        "BRANCH" -> "develop"
      )
    }
  }
}
