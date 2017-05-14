package scalakittens.experiments.word2vec

import org.specs2.mutable.Specification

import language.postfixOps
import scalakittens.{Good, IO, Strings}

class SkipGramModelTest extends Specification {

  "SkipGramModel" should {

    "process 'War And Peace'" in {

      val source = IO.linesFromResource("/warandpeace.txt")
      
      val scanner = TextScanner.WarAndPeace
      
      source map scanner.scan match {
        case Good(st) =>
 
          val model = SkipGramModel(st, 10, 0.25, 3, 10, 123456789L)
          
          model.run()
          val vectors = st.dictionary zip model.in
          println("Rare words")
          println(vectors take 10 mkString "\n")
          println("Frequent words")
          println((vectors takeRight 10).reverse mkString "\n")
          ok
          
        case bad => 
          failure(bad.listErrors.toString)
      }
      
      ok
    }
  }
}
