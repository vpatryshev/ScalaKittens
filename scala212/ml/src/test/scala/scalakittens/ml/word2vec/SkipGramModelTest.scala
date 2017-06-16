package scalakittens.ml.word2vec

import java.io.{File, FileWriter}

import org.specs2.mutable.Specification

import scala.io.Source
import scalakittens.la.{AffineTransform, Basis, Matrix, MutableVector, PCA, Vector}
import scalakittens.ml.dimreduction.{DimensionReductor, PcaDimensionReductor, SammonDimensionReductor}
import scalakittens.stats.AccumulatingMoments
import scalakittens.{Bad, Good, IO, Result}

// TODO: implement https://en.wikipedia.org/wiki/Nonlinear_dimensionality_reduction#Methods_based_on_proximity_matrices

class SkipGramModelTest extends Specification {
  val bigModelFileName = "warandpeace.vecs.txt"

  def serialize(filename: String)(vectors: List[(String, Vector)]): Unit = {
    val file = new File(s"$filename.tmp")
    file.delete()
    //    file.canWrite aka s"Cannot write to $file!" must beTrue
    val out = new FileWriter(file)
    for {
      (w, v) <- vectors
    } {
      out.write(w)
      out.write(",")
      val s = v.map(x => (x * 10000).toInt).mkString(",")
      out.write(s)
      out.write("\n")
    }
    out.close()
    file.canRead aka s"output file $file somehow disappeared" must beTrue
    file.length > 10000 aka s"output file $file too small: ${file.length}" must beTrue
    file.renameTo(new File(filename))
    ()
  }
  
  def pcaReductor(dim: Int, newDim: Int, numIterations: Int) =
    new PcaDimensionReductor(dim, newDim, precision = 0.001, numIterations)


  

  "SkipGramModel" should {

    "process 'War And Peace' slowly with PCA" in {
      val filename = bigModelFileName
      val dim = 100
      val newDim = 3
      val reductor = pcaReductor(dim, newDim, 30)
      doWarAndPeace(dim, numEpoch = 1000, filename, reductor) match {
        case Good(vs) => ok
        case bad => failure(bad.listErrors.toString)
      }

      ok
    }

    "process 'War And Peace' with PCA, fast" in {
      val filename = "warandpeace.vecs.fast.txt"
      val dim = 10
      val newDim = 3
      val reductor = pcaReductor(dim, newDim, 30)

      doWarAndPeace(dim, numEpoch = 50, filename, reductor) match {
        case Good((vs, lowdim)) =>
          val lodimVs = vs zip lowdim map {case ((n, big),small) => (n, small)}
          showWarAndPeace(lodimVs.iterator)
          ok
        case bad: Bad[_] => failure(bad.listErrors.toString + "\n" + bad.stackTrace)
      }

      ok
    }

    "process 'War And Peace' with Sammon, fast" in {
      val filename = "warandpeace.vecs.sammon.txt"
      val dim = 10
      val newDim = 3
      
      val pca = pcaReductor(dim, newDim, 30)
      val sammonReductor = new SammonDimensionReductor(dim, newDim, 30, pca)
      
      doWarAndPeace(dim, numEpoch = 50, filename, sammonReductor, 1000) match {
        case Good((vs, lowdim)) =>
          val lodimVs = vs zip lowdim map {case ((n, big),small) => (n, small)}
          showWarAndPeace(lodimVs.iterator)
          ok
        case bad: Bad[_] => failure(bad.listErrors.toString + "\n" + bad.stackTrace)
      }

      ok
    }

    "visualize War and Piece" in {
      val lines: Iterator[String] = Source.fromResource(bigModelFileName).getLines

      val found = for {
        line <- lines
        parts = line split ","
        numbers: Array[Double] = parts.tail map (_.toDouble)
        vec = Vector(numbers)
      } yield (parts.head.split(":").head, vec)

      showWarAndPeace(found)

      ok
    }
  }

  private def showWarAndPeace(found: Iterator[(String, Vector)]) = {
    val allProjections = found.map {
      case (word, vec) => (word, vec(0), vec(1))
    }.toList

//    allProjections.size must_== 17355

    val projections = allProjections.takeRight(150).reverse
    visualize("150 MOST FREQUENT WORDS", projections)
    visualize("150 MOST RARE WORDS", allProjections take 150)
  }

  private def doWarAndPeace(dim: Int, numEpoch: Int, filename: String, reductor: DimensionReductor, chunkSize: Int = 0): Result[(List[(String, Vector)],IndexedSeq[Vector])] = {
    val source = IO.linesFromResource("/warandpeace.txt")

    val scanner = TextScanner.WarAndPeace

    source map scanner.scan map {
      st =>
        val α = 0.9 / dim
        val (vectors, lowdim) = buildW2vModel(dim, numEpoch, α, st, reductor, chunkSize)
        serialize(filename)(vectors)
//        println("Rare words")
//        println(vectors take 10 mkString "\n")
//        println("Frequent words")
//        println((vectors takeRight 10).reverse mkString "\n")

        println(s"\nSEE ALL RESULTS IN $filename\n")
//        vectors.length must_== 17355
        (vectors, lowdim)
    }
  }

  private def buildW2vModel(dim: Int, numEpochs:Int, α: Double, st: ScannedText, reductor: DimensionReductor, chunkSize: Int = 0) = {
    val allOriginalVectors: Array[MutableVector] = SkipGramModel.run(dim, numEpochs, α, st)
    allOriginalVectors.foreach { v => v.isValid must beTrue; () }
    val originalVectors = if (chunkSize == 0) allOriginalVectors else allOriginalVectors.take(chunkSize)
    val vs = reductor(originalVectors)
    val vectors = st.withFrequencies zip originalVectors
    (vectors, vs)
  }

  private def applyPCA(originalVectors: IndexedSeq[Vector], newDim: Int, precision: Double, numIterations: Int) = {
    val reductor = new PcaDimensionReductor(originalVectors.head.length, newDim, precision, numIterations)
    reductor(originalVectors)
  }

  private def applySammon(originalVectors: IndexedSeq[Vector], newDim: Int, precision: Double, numIterations: Int): Unit = {
    val dim = originalVectors.head.length
    val pca = new PcaDimensionReductor(dim, newDim, precision, numIterations)
    
    new SammonDimensionReductor(dim, newDim, numIterations, pca)
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
    println
    println
    println("=" * 150)
    println(s"                                    $title\n")
    println("-" * 150)
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
        val row = samplesByLine.getOrElse(j, Nil) map (t => t._1 -> t._2)

        val layout = (Map[Int, Char]() /: row) {
          case (charMap, (w, pos)) =>
            val wordRange = math.max(pos - 1, 0) until math.min(N, pos + w.length + 1)
            if (wordRange exists charMap.contains) charMap else {
              val m1 = 0 until w.length map (i => i + pos -> w.charAt(i)) toMap

              charMap ++ m1
            }
        }

        val chars = 0 until N map (layout.getOrElse(_, ' '))

        print(chars mkString)
        println
    }
  }
}
