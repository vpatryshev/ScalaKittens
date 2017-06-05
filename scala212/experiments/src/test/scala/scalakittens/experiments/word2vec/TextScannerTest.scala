package scalakittens.experiments.word2vec

import org.specs2.mutable.Specification

import scalakittens.{Good, IO, Strings}

class TextScannerTest extends Specification {

  "TextScanner" should {

    "process 'War And Peace'" in {

      val source = IO.linesFromResource("/warandpeace.txt")

      val scanner = TextScanner.WarAndPeace

      val line1 = "“Well, Prince, so Genoa and Lucca are now just family estates of the..."
      val annaPavlovna = scanner.isBeginning(line1)
      annaPavlovna aka "Anna Pavlovna" must beTrue

      
      source map scanner.scan match {
        case Good(st) =>

          st.dictionary.length must_== 17355
          st.frequencies.length must_== st.dictionary.length
          st.dictionary.length must_== st.inverseIndex.size
          st.dictionary take 3 must_== List("arts", "diseases", "discontent")
          st.frequencies take 3 must_== List(1, 1, 1)
          st.dictionary drop (st.dictionary.length - 3) must_== List("natasha", "prince", "pierre")
          st.frequencies drop (st.dictionary.length - 3) must_== List(1213, 1928, 1963)
          st.inverseIndex("buonapartes") must_== List(6)
          st.inverseIndex("natasha") take 5 must_== List(7586, 7618, 7641, 7647, 7791)
          st.inverseIndex("natasha") takeRight 5 must_== List(257136, 257154, 257190, 257203, 257250)

          st.dictionary(st.index.head) must_== "well"

        case bad =>
          failure(bad.listErrors.toString)
      }

      ok
    }
  }
}
