package scalakittens.experiments.word2vec

import java.io.FileWriter

import org.specs2.mutable.Specification

import scala.io.Source
import scalakittens.la.{AffineTransform, Basis, Matrix, PCA, Vector}
import scalakittens.stats.AccumulatingMoments
import scalakittens.{Good, IO}

// TODO: implement https://en.wikipedia.org/wiki/Nonlinear_dimensionality_reduction#Methods_based_on_proximity_matrices

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
 
          val model = SkipGramModel(st, dim=10, α=0.09, window=3, numEpochs=50, seed=123456789L)
          model.run()
          model.in.foreach {v => v.isValid must beTrue; ()}
//System.exit(42)
          val size: Int = model.in.head.length
          val acc = AccumulatingMoments(size).collect(model.in)
          val avg = acc.avg
          val cov = acc.covariance
          println(s"avg=$avg")
          println(s"covariance=\n$cov\n\n")
          val Some(eigens) = PCA.Iterations(0.001, 10).buildEigenVectors(cov, 10)
          println("\nEIGENVALUES:\n")
          println(eigens map (_._1))
          val newBasis = Basis(avg.copy, Matrix.Unitary(eigens.map (_._2).toArray).transpose)
          
          val vs = model.in map (newBasis(_))
          
          val uvs = AffineTransform.toUnitCube(size, vs)
          val vectors = st.dictionary zip uvs
          serialize(vectors)
          println("Rare words")
          println(vectors take 10 mkString "\n")
          println("Frequent words")
          println((vectors takeRight 10).reverse mkString "\n") 

          println("\nSEE ALL RESULTS IN warandpeace.vecs.txt\n")
          vectors.length must_== 17692

          ok
          
        case bad => 
          failure(bad.listErrors.toString)
      }
      
      ok
    }
    
    "visualize War and Piece" in {
      val lines: Iterator[String] = Source.fromResource("warandpeace.vecs.txt").getLines

      val found = for {
        line <- lines
        parts = line.split(",", 2)
        vec <- Vector.read(parts(1))
      } yield (parts(0), vec) 
      
      val allProjections = found .map { 
        case (word, vec) => (word, vec(0), vec(1))
      } .toList

      allProjections.size must_== 17692

      val projections = allProjections.takeRight(150).reverse
      visualize("150 MOST FREQUENT WORDS", projections)
      visualize("150 MOST RARE WORDS", allProjections take 150)
      
      ok
    }
  }

  "visualize graphically" in {
    import scalakittens.FS._
    Folder("docs/img").mkdirs
    // this one will go to a different module
    // https://github.com/sameersingh/scalaplot
//    import org.sameersingh.scalaplot.Implicits._
//    val x = 0.0 until 2.0 * math.Pi by 0.1
//    val png = PNG("docs/img/", "test")
//
//    val tuple1: (Double=>Double, Double=>Double) = 
//      (t => math.sin(t), t => math.cos(t))
//    val tuple = x -> tuple1
//    val xyChart1 = xyChart(tuple)
//    output(png, xyChart1)
    ok
  }
  
  def visualize(title: String, projections: List[(String, Double, Double)]): Unit = {
    println; println
    println("="*150)
    println(s"                                    $title\n")
    println("-"*150)
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

    val samplesByLine = sample.groupBy(_._3) 
    
    0 to M foreach {
      j =>
        val oneLineOfTexts: Map[Int, List[String]] = samplesByLine.getOrElse(j, Nil) map { case (w, x, y) => (w, x) } groupBy (_._2) mapValues (_.map(_._1))

        val valueMap = oneLineOfTexts mapValues {
          _.head
        }

        val values = valueMap.toList.sorted

        val merged: List[(Int, String)] = (List[(Int, String)]() /: values) {
          case (doneList, wi@(i, w)) =>
            if (doneList.exists {
              case (i1, w1) =>
                i + w.length - 1 > i1 && i - 1 < i1 + w1.length
            }) doneList
            else {
              wi :: doneList
            }
        } sortBy (_._1)

        val chars: Map[Int, Char] = merged.map {
          case (i, w) => w.zipWithIndex map { case (c, i1) => (i + i1) -> c } toList
        }.flatten.toMap

        if (merged.nonEmpty) for (i <- 0 to chars.keySet.max) {
          print(chars.getOrElse(i, ' '))
        }
        println
    }
  }
}
