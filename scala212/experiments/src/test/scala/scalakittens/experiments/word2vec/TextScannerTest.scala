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

      val line2 = "Buonapartes. But I warn you, if you don’t tell me that this means war,"

      val buonaparte = Strings.normalize(line2)

      buonaparte must_== "buonapartes but i warn you if you don't tell me that this means war "

      source map scanner.scan match {
        case Good(st) =>

          st.dictionary.length must_== 17692
          st.frequencies.length must_== st.dictionary.length
          st.dictionary.length must_== st.inverseIndex.size
          st.dictionary take 3 must_== List("arts", "diseases", "discontent")
          st.frequencies take 3 must_== List(1, 1, 1)
          st.dictionary drop (st.dictionary.length - 3) must_== List("natasha", "prince", "pierre")
          st.frequencies drop (st.dictionary.length - 3) must_== List(1213, 1928, 1962)
          st.inverseIndex("buonapartes") must_== List(6)
          st.inverseIndex("natasha") take 5 must_== List(7579, 7611, 7634, 7640, 7784)
          st.inverseIndex("natasha") takeRight 5 must_== List(256797, 256815, 256851, 256864, 256911)

          st.dictionary(st.index.head) must_== "well"
//          st.dictionary(st.index(266812)) must_== "natasha"
//          st.dictionary(st.index(st.index.length - 1)) must_== "conscious"

        case bad =>
          failure(bad.listErrors.toString)
      }

      ok
    }
  }
}