package scalakittens.experiments.word2vec

import org.specs2.mutable.Specification

import language.postfixOps
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
          
          st.words.length must_== 17713
          st.frequencies.length must_== st.words.length
          st.words.length must_== st.inverseIndex.size
          st.words take 3 must_== List("arts", "diseases", "discontent")
          st.frequencies take 3 must_== List(1, 1, 1)
          st.words drop (st.words.length - 3) must_== List("natasha", "prince", "pierre")
          st.frequencies drop (st.words.length - 3) must_== List(1213, 1928, 1962)
          st.inverseIndex("buonapartes") must_== List(7)
          st.inverseIndex("natasha") take 5 must_== List(7852, 7886, 7909, 7915, 8063)
          st.inverseIndex("natasha") takeRight 5 must_== List(266793, 266812, 266851, 266866, 266915)
          
          st.positions.head must_== 3076
          
          st.positions(17710) must_== 213590
          
        case bad => 
          failure(bad.listErrors.toString)
      }
      
      ok
    }
  }
}
