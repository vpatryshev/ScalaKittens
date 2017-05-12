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
        case Good((index, words, freqs)) =>
          words.length must_== 17713
          freqs.length must_== words.length
          words.length must_== index.size
          words take 3 must_== List("arts", "diseases", "discontent")
          freqs take 3 must_== List(1, 1, 1)
          words drop (words.length - 3) must_== List("natasha", "prince", "pierre")
          freqs drop (words.length - 3) must_== List(1213, 1928, 1962)
          index("buonapartes") must_== List(7)
          index("natasha") take 5 must_== List(266915, 266866, 266851, 266812, 266793)
          
        case bad => 
          failure(bad.listErrors.toString)
      }
      
      ok
    }
  }
}
