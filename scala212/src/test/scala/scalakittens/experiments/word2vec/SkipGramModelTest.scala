package scalakittens.experiments.word2vec

import java.io.{FileWriter, FileOutputStream}

import org.specs2.mutable.Specification

import language.postfixOps
import scala.io.Source
import scalakittens.la.{Vector, Basis, Matrix, PCA}
import scalakittens.stats.AccumulatingMoments
import scalakittens.{Good, IO, Strings}

class SkipGramModelTest extends Specification {

  def serialize(vectors: List[(String, Vector)]): Unit = {
    val out = new FileWriter("warandpeace.vecs.txt")
    for {
      (w, v) <- vectors
    } {
      out.write(w)
      out.write(",")
      out.write(v.toString)
      out.write("\n")
    }
    
    out.close()
  }

  "SkipGramModel" should {

    "process 'War And Peace'" in {

      val source = IO.linesFromResource("/warandpeace.txt")
      
      val scanner = TextScanner.WarAndPeace
      
      source map scanner.scan match {
        case Good(st) =>
 
          val model = SkipGramModel(st, 10, 0.25, 3, 100, 123456789L)
          
          model.run()
          val vectors0 = st.dictionary zip model.in
          val size: Int = vectors0.head._2.length
          val acc = AccumulatingMoments(size).collect(vectors0 map (_._2))
          val avg = acc.avg
          val cov = acc.covariance
          println(s"avg=$avg")
          println(s"covariance=\n$cov\n\n")
          val Some(eigens) = PCA.Iterations(0.001, 10).buildEigenVectors(cov, 5)
          println("\nEIGENVALUES:\n")
          println(eigens map (_._1))
          val newBasis = Basis(avg, Matrix.ofColumns(avg.length, eigens.map (_._2).toArray))
          
          val vectors = vectors0 map { case (w, v) => (w, newBasis(v)) }

//          serialize(vectors)
          println("Rare words")
          println(vectors take 10 mkString "\n")
          println("Frequent words")
          println((vectors takeRight 10).reverse mkString "\n")

          println("\nSEE ALL RESULTS IN warandpeace.vecs.txt\n")

          ok
          
        case bad => 
          failure(bad.listErrors.toString)
      }
      
      ok
    }
    
    "visualize War and Piece" in {
      val lines: Iterator[String] = Source.fromFile("warandpeace.vecs.txt").getLines

      val found = for {
        line <- lines
        parts = line.split(",", 2)
        vec <- Vector.read(parts(1))
      } yield (parts(0), vec)
      
      val allProjections = found .map { 
        case (word, vec) => (word, vec(0), vec(1))
      } .toList

      allProjections.size must_== 17713

      val projections = allProjections takeRight 70
      
      val xs = projections.map(_._2)
      val ys = projections.map(_._3)
      
      val (xmin, xmax) = (xs.min, xs.max)
      val (ymin, ymax) = (ys.min, ys.max)
      
      val N = 120
      val M = 60
      val xScale = (xmax - xmin) / N
      val yScale = (ymax - ymin) / M
      
      val sample = projections map {
        case (w, x, y) => (w, ((x - xmin) / xScale).toInt + 1, ((y - ymin) / yScale).toInt)
      }

      val sortedSample = sample.sortBy {case (w, x, y) => (M - y) * N + x}

      val samplesByLine = sample.groupBy(_._3)// mapValues {case (w, x, y) => (w, x)}
      
      0 to M foreach {
        j => 
          val valueMap = samplesByLine.getOrElse(j, Nil) map {case (w, x, y) => (" " + w + " ", x-1)} groupBy(_._2) mapValues { _.head._1 }

          val values = valueMap.toList.sorted

          val split:Map[Int, Char] = values .map {
            case (i, w) => w.zipWithIndex map { case (c, i1) => (i+i1) -> c } toList 
          } .flatten .toMap
          
          print(f"$j%2d  ")
          if (values.nonEmpty) for (i <- 0 to split.keySet.max) {
            print(split.getOrElse(i, ' '))
          }
          println
      }
      
      ok
    }
  }
}
