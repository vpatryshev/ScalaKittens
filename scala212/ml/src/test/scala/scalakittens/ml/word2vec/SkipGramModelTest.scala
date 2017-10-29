package scalakittens.ml.word2vec

import java.io.{File, FileWriter}

import org.specs2.mutable.Specification

import scala.io.Source
import scalakittens.la.Spaces._
import scalakittens._
import scalakittens.la._
import scalakittens.ml.dimreduction.{DimensionReducer, PcaDimensionReducer, SammonDimensionReducer}
import scalakittens.ml.dimreduction.Viz._

// TODO: implement https://en.wikipedia.org/wiki/Nonlinear_dimensionality_reduction#Methods_based_on_proximity_matrices

class SkipGramModelTest extends Specification {
  val modelFileName = "warandpeace.vecs.txt"
  val bigModelFileName = "warandpeace.vecs.txt"
  
  def serialize[Space <: VectorSpace](filename: String, space: Space, vectors: List[(String, Space#Vector)]): Unit = {
    val file = new File(s"$filename.tmp")
    file.delete()
    //    file.canWrite aka s"Cannot write to $file!" must beTrue
    val out = new FileWriter(file)
    out.write(s"dim=${space.dim}\n")
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
    val minSize = 20 * vectors.length
    file.length > minSize aka s"output file $file too small: ${file.length}, expected at least $minSize" must beTrue
    file.renameTo(new File(modelFileName))
    ()
  }
  
  def pcaReducer(source: VectorSpace, target: VectorSpace, numIterations: Int) =
    new PcaDimensionReducer[source.type, target.type](source, target, precision = 0.001, numIterations)

  sequential
  
  "SkipGramModel" should {

//    "process 'War And Peace' slowly with PCA" in {
//      val filename = bigModelFileName
//      filename == null must beFalse
//      val dim = R100
//      val newDim = R3
//      val reducer = pcaReducer(dim, newDim, 30)
//      reducer == null must beFalse
//      doWarAndPeace(dim, numEpoch = 1000, newDim, filename, reducer) match {
//        case Good(vs) => ok
//        case bad => failure(bad.listErrors.toString)
//      }
//
//      ok
//    }

    "process 'War And Peace' with PCA, fast" in {
      val filename = "warandpeace.vecs.fast.txt"
      val reducer: DimensionReducer[R10.type, R3.type] = new PcaDimensionReducer[R10.type, R3.type](R10, R3, precision = 0.001, 30)//pcaReducer(dim, newDim, 30)
      
      doWarAndPeace[R10.type, R3.type](reducer, filename, numEpoch = 50) match {
        case Good(vs) =>
          showWarAndPeace[R3.type](vs.iterator)
          ok
        case bad: Bad[_] => failure(bad.listErrors.toString + "\n" + bad.stackTrace)
        case Empty => failure("No War, no Peace! /* Trotsky */")
      }

      ok
    }

    "process 'War And Peace' with Sammon, slow" in {
      val filename = "warandpeace.vecs.sammon.txt"
      val t = new Tracker
      val sammonReducer: DimensionReducer[R7.type, R2.type] = SammonDimensionReducer.withPCA[R7.type, R2.type](R7, R2, 20)
      t << "instantiated sammon reducer"
      doWarAndPeace[R7.type, R2.type](sammonReducer, filename, numEpoch = 50) match {
        case Good(vs: List[(String, R2.Vector)]) =>
          t << s"did war and peace, good (${vs.length} words"
          showWarAndPeace[R2.type](vs.iterator)
          ok
        case bad: Bad[_] =>
          t << s"did not do war and peace"
          failure(bad.listErrors.toString + "\n" + bad.stackTrace)
        case Empty => failure("No War, no Peace! /* Trotsky */")
      }

      ok
    }

    "process 'War And Peace' with Sammon, fast" in {
      val filename = "warandpeace.vecs.sammon.txt"

      val sammonReducer = SammonDimensionReducer.withPCA[R10.type, R3.type](R10, R3, 30)

      doWarAndPeace[R10.type, R3.type](sammonReducer, filename, numEpoch = 50, 1000) match {
        case Good(vs: List[(String, R3.Vector)]) =>
          showWarAndPeace[R3.type](vs.iterator)
          ok
        case bad: Bad[_] => failure(bad.listErrors.toString + "\n" + bad.stackTrace)
        case Empty => failure("No War, no Peace! /* Trotsky */")
      }

      ok
    }

    "visualize War and Piece" in {
      val lines: Iterator[String] = Source.fromResource(modelFileName).getLines
      val Header = "dim=(\\d+)".r
      val Header(dim) = lines.next
      val space = VectorSpace(dim.toInt)
 
      val found = for {
        line <- lines
        parts = line split ","
        numbers:Array[Double] = parts.tail map (_.toDouble)
        vec = space.Vector(numbers)
      } yield (parts.head.split(":").head, vec)


      showWarAndPeace[space.type](found)

      ok
    }
  }

  private def showWarAndPeace[Space <: VectorSpace](found: Iterator[(String, Space#Vector)]) = {
    val allProjections = found.map {
      case (word, vec) => (word, vec(0), vec(1))
    }.toList

//    allProjections.size must_== 17355
    val nWords = 120
    val projections = allProjections.takeRight(nWords).reverse
    visualize(s"$nWords MOST FREQUENT WORDS", projections)
    visualize(s"$nWords MOST RARE WORDS", allProjections take nWords)
  }

  private def doWarAndPeace[S <: VectorSpace, T <: VectorSpace](
      reducer: DimensionReducer[S, T],
      filename: String,
      numEpoch: Int, 
      chunkSize: Int = 0): Result[List[(String, T#Vector)]] =
    doNovel(WarAndPeace("/warandpeace.txt"), reducer, filename, numEpoch, chunkSize)

  private def doNovel[S <: VectorSpace, T <: VectorSpace](
      scanner: TextScanner,
      reducer: DimensionReducer[S, T],
      filename: String,
      numEpoch: Int,
      chunkSize: Int = 0): Result[List[(String, T#Vector)]] = {

    scanner.scannedText map {
      st =>
        val α = 0.9 / reducer.source.dim
        val vectors: List[(String, reducer.target.Vector)] = 
          buildModel[S, T](st, reducer, numEpoch, α, chunkSize)
        serialize(filename, reducer.target, vectors)
        println("Rare words")
        println(vectors take 10 mkString "\n")
        println("Frequent words")
        println((vectors takeRight 10).reverse mkString "\n")

        println(s"\nSEE ALL RESULTS IN $filename\n")
        //        vectors.length must_== 17355
        vectors
    }
  }

  private def buildModel[S <: VectorSpace, T <: VectorSpace](
      st: ScannedText, 
      reducer: DimensionReducer[S, T],
      numEpochs:Int, 
      α: Double, 
      chunkSize: Int = 0): List[(String, reducer.target.Vector)] = {
    val allOriginalVectors: Array[reducer.source.Vector] = runSkipGram(reducer.source, numEpochs, α, st)
    val originalVectors = if (chunkSize == 0) allOriginalVectors else allOriginalVectors.takeRight(chunkSize)
    val vs = reducer.reduce(originalVectors)
    val uvs = reducer.target.toUnitCube(vs map (_.asInstanceOf[reducer.target.Vector]))
    val vectors = st.withFrequencies zip uvs
    vectors
  }

  private def runSkipGram[Space <: VectorSpace](dim: Space, numEpochs: Int, α: Double, st: ScannedText): Array[dim.Vector] = {
    val model = SkipGramModel(st, dim, numEpochs, window = 3, α, seed = 123456789L)
    model.run()
    val originalVectors = model.in
    originalVectors.zipWithIndex foreach { 
      case(v,i) => v.isValid aka s"@$i: $v" must beTrue; () }
    originalVectors.asInstanceOf[Array[dim.Vector]] // TODO: get rid of casting
  }

//  private def applyPCA(originalVectors: IndexedSeq[Vector], newDim: VectorSpace, precision: Double, numIterations: Int) = {
//    val reducer = new PcaDimensionReducer(originalVectors.head.length, newDim, precision, numIterations)
//    reducer.reduce(originalVectors)
//  }
//
//  private def applySammon(originalVectors: IndexedSeq[Vector], newDim: VectorSpace, precision: Double, numIterations: Int): Unit = {
//    val dim = originalVectors.head.length
//    val pca = new PcaDimensionReducer(dim, newDim, precision, numIterations)
//    
//    new SammonDimensionReducer(dim, newDim, numIterations, pca)
//  }
  
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

}
