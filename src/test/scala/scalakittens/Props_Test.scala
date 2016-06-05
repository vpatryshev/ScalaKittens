package scalakittens

import org.specs2.mutable.Specification
import Props._

class Props_Test extends Specification {

  "this test" should {
    "fail on failure" in {
      val x = List(1,2,3)
//      x.isEmpty must beTrue
      val f = false
//      false must beTrue
      ok
    }
  }

  "PropsOps.replaceAll" should {
    "keep intact strings that don't match" in {
      replaceAll(Map("abc" -> "<<<$1>>>"))("not an ABC") must_== "not an ABC"
    }
    "transform strings according to patterns" in {
      replaceAll(Map(".*(abc).*" -> "<<<$1>>>"))("this is an abc!") must_== "<<<abc>>>"
    }
  }

  "Props.@@" should {
    "not crash on four parameters" in {
      val pf = props("a.b.c.d" -> "<<A B C D>>")
      val res = pf.valueOf("x", "b", "c", "d")
      ok
    }

    "pass a negative real-life case" in {
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
      val mr = Props.keyMatches(key)("Private.Abroad.Bribe.Yearly")
      mr must beFalse
      val mks: Iterable[String] = sut.matchingKeys(key)
      mks.toList.length must_== 0
      val key1: Option[String] = sut.findKey(key)
      key1 must_== None
      val bad = sut valueOf key
      bad.isBad aka bad.toString must beTrue
      bad.toString contains "Missing 'Private.InTheHouse.Bribe.sofar'" must beTrue
    }

    "pass a positive real-life case" in {
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

    "pass a positive real-life case with transformer" in {
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
      val actual1 = sut valueOf("Private", "InTheHouse", "Bribe", "sofar")
      actual1 aka sut.toString must_== Result.forValue("$15.00")

      val actual2 = sut valueOf("Private", "Somewhere", "Alcohol", "max")
      actual2 aka sut.toString must_== Result.forValue("$30.00")
    }

    "pass a positive real-life case with regex" in {
      val transformer: String => String = replaceAll("Used" -> "sofar", "Yearly" -> "max", "Out-of-(\\w+)" -> "Out$1", "In-The-(\\w+)" -> "InThe$1")
      val sut = props("Private.In-The-House.Alcohol.Yearly" -> "$30.00",
        "Private.Abroad.Bribe.Used" -> "$0.00",
        "Private.In-The-House.Bribe.Used" -> "$15.00",
        "Private.In-The-House.Bribe.Yearly" -> "$15.00",
        "Private.In-The-House.Alcohol.Used" -> "$1734.48",
        "Private.Abroad.Bribe.Yearly" -> "$15.00",
        "Private.Abroad.Alcohol.Yearly" -> "$30.00",
        "Private.Abroad.Alcohol.Used" -> "$0.00")(transformer)
      sut valueOf("Private", "InTheHouse", "Bribe", "sofar") aka sut.toString must_== Result.forValue("$15.00")
      sut valueOf("Private", "Abroad", "Alcohol", "max") aka sut.toString must_== Result.forValue("$30.00")
    }
  }

  "Props" should {
    "addPrefix" in {
      val fp: Props = props("a" -> "1", "b" -> "2")
      val sut = fp.addPrefix(":)")
      sut.isDefinedAt(":).a") must beTrue
      sut.isDefinedAt(":).b") must beTrue
      sut.isDefinedAt(":).c") must beFalse
      sut.isDefinedAt(":(.a") must beFalse
      sut(":).a") must_== "1"
      sut(":).b") must_== "2"
    }
    "++" in {
      val fp1: Props = props("a" -> "1", "b" -> "2")
      val fp2: Props = props("b" -> "42", "c" -> "4")
      val sut = fp1 ++ fp2
      sut.isDefinedAt("a") must beTrue
      sut.isDefinedAt("b") must beTrue
      sut.isDefinedAt("c") must beTrue
      sut.isDefinedAt("d") must beFalse
      sut("a") must_== "1"
      sut("b") == "2" || sut("b") == "42" must beTrue
      sut("c") must_== "4"
    }

    "accumulate" in {
      val x = accumulate(Nil)
      x.toString must_== "Empty()"
      val fp1: Props = props("a" -> "1", "b" -> "2")
      val fp2: Props = props("b" -> "42", "c" -> "4")
      val fp3: Props = props("c" -> "88", "d" -> "6")
      val sut = accumulate(fp1 :: fp2 :: fp3 :: Nil)
      sut.isDefinedAt("a") must beTrue
      sut.isDefinedAt("b") must beTrue
      sut.isDefinedAt("c") must beTrue
      sut.isDefinedAt("d") must beTrue
      sut.isDefinedAt("e") must beFalse
      sut("a") must_== "1"
      sut("b") == "2" || sut("b") == "42" must beTrue
      sut("c") == "4" || sut("c") == "88" must beTrue
      sut("d") must_== "6"
    }

    "@@ behave on four parameters" in {
      val pf = props("a.b.c.d" -> "<<A B C D>>")
      pf valueOf("a", "b", "c", "d") must_== Good("<<A B C D>>")
      (pf valueOf("x", "b", "c", "d")).isBad must beTrue
    }

    "work with regexes" in {
      val pf = props("a1.b2.c3.d4" -> "<<A B C D>>")
      val actual: Result[String] = pf @@("a?", "b?", "c3", "d?")
      actual must_== Good("<<A B C D>>")
      (pf @@("x", "b", "c", "d")).isBad must beTrue
    }

    "reorder params" in {
      val pf = props("a.b.c.d" -> "<<A B C D>>") ++ props("a1.b1.c1.d1" -> "<<A1 B1 C1 D1>>")
      val reordered = pf reorder(4, 2, 1, 3)
      reordered valueOf("c", "b", "d", "a") must_== Good("<<A B C D>>")
      reordered valueOf("c1", "b1", "d1", "a1") must_== Good("<<A1 B1 C1 D1>>")
      (reordered valueOf("a", "b", "c", "d")).isBad must beTrue
    }

    "reorder params, heterogeneous" in {
      val pf = props("a.b.c.d" -> "<<A B C D>>")
      val sut = (pf reorder(4, 2, 1, 3)) ++ props("clue" -> "treasure")
      val res = sut valueOf("c", "b", "d", "a")
      res must_== Good("<<A B C D>>")
      (sut valueOf("a", "b", "c", "d")).isBad must beTrue
      sut valueOf "clue" must_== Good("treasure")
    }

    "dropPrefix work on four parameters" in {
      val pf = props("a.b.c.d" -> "<<ABCD>>", "x" -> "<<X>>")
      val dp = pf.dropPrefix
      dp @@("b", "c", "d") must_== Good("<<ABCD>>")
      (dp @@ "x").isBad must beTrue
    }

    "find having" in {
      val sut = props("a.b.c.d" -> "<<ABCD>>", "x" -> "<<X>>", "a.d.x.y" -> "<<ADXY>>")
      sut.findHaving("c") must_== Good("<<ABCD>>")
      sut.findHaving("a").isBad must beTrue
      sut.findHaving("ab").isBad must beTrue
      sut.findHaving("y.d") must_== Good("<<ADXY>>")
    }

    "find and replace" in {
      val sut1 = props("a.b.c.d" -> "<<ABCD>>", "x" -> "<<X>>", "a.d.x.y" -> "<<ADXY>>")
      val sut2 = props("a.b.c.d" -> "<<ABCDZZ>>", "x" -> "<<X>>", "a.d.x.y" -> "<<ADXY>>")
      val replaced = sut1.findAndReplace("c", "<<ABCDZZ>>")
      replaced == sut2 must beTrue

      sut1.findAndReplace("ab", "oh really?!") must_== sut1
      val sut3 = Props parse "fp(Map(\"Back to Transaction History.Account #2014151 Details.Health Net ID#\" -> \"R0000000-00\", \"Back to Transaction History.Account #20140101 Details.Name\" -> \"SCOTT GRISCHY\"))"
      sut3 match {
        case Good(props) =>
          val newOne = props.findAndReplace("Name", "Lia Van Damm")
          val newName: Result[String] = newOne.findHaving("Name")
          newName must_== Good("Lia Van Damm")
        case bad => failure(s"wrong text: $bad")
      }
      ok
    }
    //
    "discard empty subtrees" in {
      val sut = props("a.b.c.d" -> "<<ABCD>>", "x" -> "<<X>>", "a.d.x.y" -> "<<ADXY>>")
      sut.findAllHaving("d").toString must_== """fp(Map("a.b.c.d" -> "<<ABCD>>", "a.d.x.y" -> "<<ADXY>>"))"""
    }

    "extract one by subkey" in {
      val sut = props("a.b.[[1]].c.d" -> "<<ABCD>>", "a.b.[[1]].c.y" -> "<<ABCY>>", "x" -> "<<X>>")
      sut.extractAllNumberedHaving("d").toList.toString must_== """List(fp(Map("c.d" -> "<<ABCD>>", "c.y" -> "<<ABCY>>")))"""
    }
    "trim prefixes wisely" in {
      val p = props("a.b.c" -> "ABC")
      val t = p.trimPrefixes
      val s = t.toString()
      s must_== props("c" -> "ABC").toString
      props("a.b" -> "ABC").trimPrefixes.toString must_== props("b" -> "ABC").toString
      props("a" -> "ABC").trimPrefixes.toString must_== props("a" -> "ABC").toString
    }

    "return a subtree" in {
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

    "return a set of keys" in {
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

    "produce a subtree of ending with" in {
      val sut = props("a.b.c" -> "<<ABC>>", "c" -> "<<C>>").endingWith("c")
      sut.toString must_== """fp(Map("a.b" -> "<<ABC>>"))"""
    }
  }

  "fromParallelLists" should {
    "extract data when headers are the same" in {
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
    "extract data when headers are messed up" in {
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

    "fail if headers are missing" in {
      val got = Props.fromParallelLists(List("A", "C"), List("a1", "c1"), List("A", "B", "C"))
      got match {
        case bad: Bad[_] => bad.toString() contains "(b)" must beTrue
        case shozanah => failure(s"Expected an error in $shozanah")
      }
      ok
    }
  }

  "From Map " should {
    "drop indexes" in {
      val sut0 = props("a.b.[[1]].c.d" -> "<<ABCD>>", "a.b.[[1]].c.y" -> "<<ABCY>>", "x" -> "<<X>>")
      val sut = sut0.dropIndexes
      sut @@ "a.b.c.d" must_== Good("<<ABCD>>")
      sut @@ "a.b.c.y" must_== Good("<<ABCY>>")
      sut @@ "x" must_== Good("<<X>>")
    }
  }

  "transformKeys" should {
    "work" in {
      val sut0 = props("a" -> "1", "b" -> "2")
      val sut = sut0.transformKeys("<<" + _ + ">>")
      sut must_== props("<<a>>" -> "1", "<<b>>" -> "2")
    }
  }

  "Parser" should {
    "parse simple sample as text" in {
      val sutOpt = Props parse "fp(Map(\"a\" -> \"b\"))"
      sutOpt match {
        case Good(sut) =>
          sut @@ "a" must_== Good("b")
        case orelse => failure(s"Oops, not good: $orelse")
      }
      ok
    }

    "have left inverse" in {
      val txt = props("a" -> "b").toString()
      val sutOpt = Props parse txt
      sutOpt match {
        case Good(sut) =>
          sut @@ "a" must_== Good("b")
        case orelse => failure(s"Oops, not good: $orelse in $txt")
      }
      ok
    }

    "parse simple sample with dictionary as text" in {
      val sutOpt = Props parse "fp(Map(\"a\" -> \"b\")) with dictionary Map(\"x\" -> \"y\", \"z\" -> \"a\")"
      sutOpt match {
        case Good(sut) =>
          sut @@ "z" must_== Good("b")
        case orelse => failure(s"Oops, not good: $orelse")
      }
      ok
    }

    "parse simple sample with reordering as text" in {
      val sutOpt = Props parse "fp(Map(\"a.b.c.d\" -> \"cdba\")) with reordering (3,4,2,1)"
      sutOpt match {
        case Good(sut) =>
          sut @@ "d.c.a.b" must_== Good("cdba")
        case orelse => failure(s"Oops, not good: $orelse")
      }
      ok
    }

    "parse simple sample with dictionary and reordering as text" in {
      val sutOpt = Props parse "fp(Map(\"a.b.c.d\" -> \"cdba\")) with dictionary Map(\"x\" -> \"y\", \"z\" -> \"a\") with reordering (3,4,2,1)"
      sutOpt match {
        case Good(sut) =>
          sut @@ "d.c.z.b" must_== Good("cdba")
        case orelse => failure(s"Oops, not good: $orelse")
      }
      ok
    }

    "parse OOP sample as text" in {
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

    "parse suspicious Wednesday Addams data" in {
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

  "all these functions" should {
    "Keep props intact in groupByIndex if there are no indexes" in {
      val pp = props(Map("key1" -> "val1", "key2" -> "val2"))
      val sut = pp.groupByIndex
      sut.size must_== 1
      sut(0) must_== pp
    }

    "Group by index in simple case" in {
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

    "Group by index in double index case" in {
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

    "Behave in a modified regression case of embedded array" in {

      val text05062014 = "fp(Map(\"[[1]].[[1]].K\" -> \"I0\", \"[[3]].[[1]].K\" -> \"I2\", \"[[2]].[[1]].K\" -> \"I1\"))"
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
      ok
    }

    "parse when keys contain commas" in {
      val text = "fp(Map(\"DoS\" -> \"01/29/2014\", \"HERMIONE GRANGER, MUGGLE.Passport Number\" -> \"9\"))"
      val propsOpt = Props parse text
      propsOpt match {
        case Good(props) =>
          props @@ "HERMIONE GRANGER, MUGGLE.Passport Number" must_== Good("9")
        case basura => failure(s"Oops, not good: $basura")
      }
      ok
    }

    "Group by index using real data" in {
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

    "Group by index in transitional" in {
      val text = "fp(Map(\"[[2]].[[1]].Tip\" -> \"$0.00\", \"[[3]].[[1]].Distance Limit\" -> \"$27.13\", \"[[1]].[[1]].Distance Flown by your Best Friend\" -> \"$0.00\", \"[[3]].[[1]].Departure Date\" -> \"10/16/2013 - 10/16/2013\", \"[[3]].[[1]].Distance Flown by your Best Friend\" -> \"$0.00\", \"[[1]].[[1]].key01\" -> \"val01\", \"[[2]].[[1]].Distance Limit\" -> \"$16.00\", \"[[3]].[[1]].Wasted\" -> \"$0.00\", \"[[2]].[[1]].Departure Date\" -> \"10/16/2013 - 10/16/2013\", \"[[3]].[[1]].Fuel Burned\" -> \"$38.00\", \"[[1]].[[1]].Wasted\" -> \"$91.49\", \"[[2]].[[1]].Wasted\" -> \"$0.00\", \"[[3]].[[1]].key21\" -> \"val21\", \"[[2]].[[1]].Distance Flown by your Best Friend\" -> \"$0.00\", \"[[1]].[[1]].Tax\" -> \"$0.00\", \"[[2]].[[1]].key11\" -> \"val11\", \"[[2]].[[1]].Tax\" -> \"$0.00\", \"[[1]].[[1]].Distance Flown by your Traditional Partner\" -> \"$0.00\", \"[[3]].[[1]].Distance Flown by your Traditional Partner\" -> \"$27.13\", \"[[1]].[[1]].Departure Date\" -> \"10/16/2013 - 10/16/2013\", \"[[2]].[[1]].Fuel Burned\" -> \"$30.00\", \"[[3]].[[1]].Tip\" -> \"$0.00\", \"[[1]].[[1]].Distance Limit\" -> \"$91.49\", \"[[2]].[[1]].Distance Flown by your Traditional Partner\" -> \"$16.00\", \"[[1]].[[1]].Flyweight\" -> \"$91.49\", \"[[2]].[[1]].Flyweight\" -> \"$0.00\", \"[[1]].[[1]].Fuel Burned\" -> \"$144.00\", \"[[3]].[[1]].Flyweight\" -> \"$0.00\", \"[[3]].[[1]].Tax\" -> \"$0.00\", \"[[1]].[[1]].Tip\" -> \"$0.00\"))"
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

    "Behave in a regression of 10/16/2013" in {
      val text = "fp(Map(\"[[2]].[[1]].Tip\" -> \"$0.00\", \"[[3]].[[1]].Distance Limit\" -> \"$27.13\", \"[[1]].[[1]].Distance Flown by your Best Friend\" -> \"$0.00\", \"[[3]].[[1]].Departure Date\" -> \"10/16/2013 - 10/16/2013\", \"[[3]].[[1]].Distance Flown by your Best Friend\" -> \"$0.00\", \"[[1]].[[1]].Purpose of the Trip\" -> \"Private Beach Party\", \"[[2]].[[1]].Distance Limit\" -> \"$16.00\", \"[[3]].[[1]].Wasted\" -> \"$0.00\", \"[[2]].[[1]].Departure Date\" -> \"10/16/2013 - 10/16/2013\", \"[[3]].[[1]].Official Distance\" -> \"$38.00\", \"[[1]].[[1]].Wasted\" -> \"$91.49\", \"[[2]].[[1]].Wasted\" -> \"$0.00\", \"[[3]].[[1]].Purpose of the Trip\" -> \"Private Show\", \"[[2]].[[1]].Distance Flown by your Best Friend\" -> \"$0.00\", \"[[1]].[[1]].Tax\" -> \"$0.00\", \"[[2]].[[1]].Purpose of the Trip\" -> \"Thai Massage\", \"[[2]].[[1]].Tax\" -> \"$0.00\", \"[[1]].[[1]].Distance Flown by your Traditional Partner\" -> \"$0.00\", \"[[3]].[[1]].Distance Flown by your Traditional Partner\" -> \"$27.13\", \"[[1]].[[1]].Departure Date\" -> \"10/16/2013 - 10/16/2013\", \"[[2]].[[1]].Fuel Burned\" -> \"$30.00\", \"[[3]].[[1]].Tip\" -> \"$0.00\", \"[[1]].[[1]].Distance Limit\" -> \"$91.49\", \"[[2]].[[1]].Distance Flown by your Traditional Partner\" -> \"$16.00\", \"[[1]].[[1]].Flyweight\" -> \"$91.49\", \"[[2]].[[1]].Flyweight\" -> \"$0.00\", \"[[1]].[[1]].Fuel Burned\" -> \"$144.00\", \"[[3]].[[1]].Flyweight\" -> \"$0.00\", \"[[3]].[[1]].Tax\" -> \"$0.00\", \"[[1]].[[1]].Tip\" -> \"$0.00\"))"
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

    "regression 1 of 07/03/2014" in {
      val text = "fp(Map(\"DoS\" -> \"01/29/2014\", \"HERMIONE GRANGER (MUGGLE).Passport Number\" -> \"9\"))"
      val propsOpt = Props parse text
      propsOpt match {
        case Good(props) =>
          props @@ ".Passport Number" must_== Good("9")
        case basura => failure(s"Oops, not good: $basura")
      }
      ok
    }

    "Extract data from list" in {
      fromTree("first" :: "second" :: Nil) must_== Good(props("[[1]]" -> "first", "[[2]]" -> "second"))
    }

    "parse pair with a list as value" in {
      isPrimitive("first" :: "second" :: Nil) must beFalse
      val actual = parsePair("key", "first" :: "second" :: Nil)
      actual must_== Good(props("key.[[1]]" -> "first", "key.[[2]]" -> "second"))
    }

    "Extract data from list in map" in {
      val actual = fromTree(Map("key" -> ("first" :: "second" :: Nil)))
      actual must_== Good(props("key.[[1]]" -> "first", "key.[[2]]" -> "second"))
    }

    "Extract data from full tree" in {
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
          p @@ "abroad.individualHome" aka p.toString must_== Good("7000")
        case oops => failure(s"oops: $oops")
      }
      ok

    }

  }
}
