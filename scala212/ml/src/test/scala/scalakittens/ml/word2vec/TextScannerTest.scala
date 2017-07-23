package scalakittens.ml.word2vec

import org.specs2.mutable.Specification

import scalakittens._

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

          st.dictionary.length must_== 17210
          st.frequencies.length must_== st.dictionary.length
          st.dictionary.length must_== st.inverseIndex.size
          st.dictionary take 3 must_== List("arts", "diseases", "discontent")
          st.frequencies take 3 must_== List(1, 1, 1)
          st.dictionary drop (st.dictionary.length - 3) must_== List("natasha", "prince", "pierre")
          st.frequencies drop (st.dictionary.length - 3) must_== List(1213, 1928, 1963)
          st.inverseIndex("buonapartes") must_== List(5)
          st.inverseIndex("natasha") take 5 must_== List(6361, 6387, 6405, 6411, 6537)
          st.inverseIndex("natasha") takeRight 5 must_== List(214570, 214582, 214605, 214614, 214647)

          st.dictionary(st.index.head) must_== "prince" // "Eh bien, mon prince..."

        case bad =>
          failure(bad.listErrors.toString)
      }

      ok
    }
  }
}
