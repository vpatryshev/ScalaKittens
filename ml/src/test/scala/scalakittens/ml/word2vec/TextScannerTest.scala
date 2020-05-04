package scalakittens.ml.word2vec

import org.specs2.mutable.Specification

import scalakittens._

class TextScannerTest extends Specification {

  "TextScanner" should {
    "process 'War And Peace'" in {
      val novel = WarAndPeace
      val line1 = "“Well, Prince, so Genoa and Lucca are now just family estates of the..."
      val annaPavlovna = novel.isBeginning(line1)
      annaPavlovna aka "Anna Pavlovna" must beTrue

      novel.scannedText match {
        case Good(st) ⇒

          math.abs(st.dictionary.length - 17210) < 20 aka s"actually, ${st.dictionary.length}" must beTrue
          st.frequencies.length must_== st.dictionary.length
          st.dictionary.length must_== st.inverseIndex.size
          st.dictionary take 3 must_== List("arts", "diseases", "discontent")
          st.frequencies take 3 must_== List(1, 1, 1)
          st.dictionary drop (st.dictionary.length - 3) must_== List("natasha", "prince", "pierre")
          st.frequencies drop (st.dictionary.length - 3) must_== List(1213, 1928, 1963)
          st.inverseIndex("buonapartes") must_== List(5)
          st.inverseIndex("natasha") take 5 must_== List(6455, 6481, 6500, 6506, 6633)
          st.inverseIndex("natasha") takeRight 5 must_== List(218040, 218052, 218074, 218082, 218115)

          st.dictionary(st.index.head) must_== "prince" // "Eh bien, mon prince..."

        case bad ⇒
          failure(bad.listErrors.toString)
      }

      ok
    }

    "process 'Gone With The Wind'" in {
      val novel = GoneWithTheWind
      val line1 = "Scarlett O'Hara was not beautiful, but men seldom realized it when"
      val scarlett = novel.isBeginning(line1)
      scarlett aka "Scarlett O'Hara" must beTrue
      
      novel.scannedText match {
        case Good(st) ⇒

          math.abs(st.dictionary.length - 16194) < 30 aka s"actually, ${st.dictionary.length}" must beTrue
          st.frequencies.length must_== st.dictionary.length
          st.dictionary.length must_== st.inverseIndex.size
          st.dictionary take 3 must_== List("looms", "motionless", "onter")
          st.frequencies take 3 must_== List(1, 1, 1)
          st.dictionary drop (st.dictionary.length - 3) must_== List("ashley", "melanie", "scarlett")
          st.frequencies drop (st.dictionary.length - 3) must_== List(983, 1020, 2781)
          st.inverseIndex("damn") take 5 must_== List(2157, 2377, 7493, 26385, 31512)

          st.dictionary(st.index.head) must_== "scarlett" // "Eh bien, mon prince..."

        case bad ⇒
          failure(bad.listErrors.toString)
      }

      ok
    }
  }
}
