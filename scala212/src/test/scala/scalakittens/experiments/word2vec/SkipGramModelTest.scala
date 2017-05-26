package scalakittens.experiments.word2vec

import org.specs2.mutable.Specification

import language.postfixOps
import scalakittens.la.{Basis, Matrix, PCA}
import scalakittens.stats.AccumulatingMoments
import scalakittens.{Good, IO, Strings}

class SkipGramModelTest extends Specification {

  "SkipGramModel" should {

    "process 'War And Peace'" in {

      val source = IO.linesFromResource("/warandpeace.txt")
      
      val scanner = TextScanner.WarAndPeace
      
      source map scanner.scan match {
        case Good(st) =>
 
          val model = SkipGramModel(st, 10, 0.25, 3, 100, 123456789L)
          
          model.run()
          val vectors = st.dictionary zip model.in
          println("Rare words")
          println(vectors take 10 mkString "\n")
          println("Frequent words")
          println((vectors takeRight 10).reverse mkString "\n")
          val size: Int = vectors.head._2.length
          val acc = AccumulatingMoments(size).update(vectors map (_._2))
          val avg = acc.avg
          val cov = acc.covariance
          println(s"avg=$avg")
          println(s"covariance=\n$cov\n\n")
          val Some(eigens) = PCA.Iterations(0.001, 100).buildEigenVectors(cov, 10)
          println("\nEIGENVALUES:\n")
          println(eigens map (_._1))
          val newBasis = Basis(avg, Matrix.ofColumns(avg.length, eigens.map (_._2).toArray))          
          ok
          
        case bad => 
          failure(bad.listErrors.toString)
      }
      
      ok
    }
  }
}
