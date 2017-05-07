package scalakittens.experiments.word2vec

import java.text.Normalizer

import org.specs2.mutable.Specification

import scala.collection.mutable
import scalakittens.{Strings, Good, IO}

class HuffmanTreeTest extends Specification {

  "HuffmanTree" should {

    "build a tree" in {
      // source: https://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html
      val sut = new HuffmanTree(5::7::10::15::20::45::Nil)
// this is an artistic graph representation
      (0 until sut.size map sut.parent).toList must_== List(6,-6,7,8,-8,10,-7,9,-9,-10,0)
      
      sut.chain(0) must_== List((0,5), (6,12), (7,22), (9,57), (10,102))
      sut.chain(1) must_== List((1,7), (6,12), (7,22), (9,57), (10,102))
      sut.chain(2) must_== List((2,10), (7,22), (9,57), (10,102))
      sut.chain(3) must_== List((3,15), (8,35), (9,57), (10,102))
      sut.chain(4) must_== List((4,20), (8,35), (9,57), (10,102))
      sut.chain(5) must_== List((5,45), (10,102))
      
      sut.freq.toList must_== List(5,7,10,15,20,45,12,22,35,57,102)
      sut.toGraph must_==
        """    102:10
          |   /      \___________
          |  /                   \
          |45:5                57:9
          |            _______/    \__
          |           /               \
          |          22:7            35:8
          |         /    \          /   \
          |      10:2     12:6   15:3  20:4
          |              /  \
          |            5:0  7:1""".stripMargin
      ok
    }
    
    "render sassa_nf sample" in {
      val sut = new HuffmanTree(List(2,4,4,5,7,8))
      sut.toGraph must_==
        """             30:10
          |          __/     \
          |         /         \
          |        13:8      17:9
          |     __/  \      /    \
          |    /      \   8:5     9:7
          |   6:6    7:4         /  \
          |  /  \              4:2  5:3
          |2:0  4:1""".stripMargin
      ok
    }

    "process War And Peace" in {
      val counters = new mutable.HashMap[String, Int] withDefaultValue 0
      def isBeginning(line: String) = line matches "\\s*.{5,10}[Pp]rince.*"
      def isEnd(line: String) = line contains "End of the Project Gutenberg EBook"
      def isHeader(line: String) = line matches "\\s*CHAPTER [CILVX]+\\s*"
      val source = IO.linesFromResource("/warandpeace.txt")
      
      val line1 = "“Well, Prince, so Genoa and Lucca are now just family estates of the..."
      val annaPavlovna = isBeginning(line1) 
      annaPavlovna aka "Anna Pavlovna" must beTrue
      
      val line2 = "Buonapartes. But I warn you, if you don’t tell me that this means war,"
//      
//      line2.toLowerCase.replaceAll("[^\\w'`]+", " ") must_== "buonapartes but i warn you  if you don’t tell me that this means war "


      val buonaparte = Strings.normalize(line2)
      
      buonaparte must_== "buonapartes but i warn you if you don't tell me that this means war "
      
      def extractNovel(text: Iterator[String]): Iterator[String] = text dropWhile (!isBeginning(_)) takeWhile (!isEnd(_)) filterNot isHeader
      
      source map extractNovel match {
        case Good(text) =>
          for {
            l <- text map Strings.normalize
            word <- l.split(" ") filterNot Strings.isStop
          } {
            counters(word) += 1
            word == "ti'" aka l must beFalse
          }
        case bad => failure(bad.listErrors.toString)
      }
      val cc = counters.toList.sortBy(_._2).zipWithIndex
      val dict: Map[String, Int] = {
        for {e: ((String, Int), Int) <- cc} yield e._1._1 -> e._2
      } toMap
      
      
      val freq: List[Int] = {
        for {e: ((String, Int), Int) <- cc} yield e._1._2
      } toList
      
      val tree = new HuffmanTree(freq)
      
      println(cc.size)
      println(tree.chain(1000))
//      println(tree.toGraph)
      
//      failure(cc.toString)
      ok
    }
  }
}
