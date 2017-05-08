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
      
      sut.path(0) must_== List(0, 6, -7, 9, -10)
      sut.path(1) must_== List(1, -6, -7, 9, -10)
      sut.path(2) must_== List(2, 7, 9, -10)
      sut.path(3) must_== List(3, 8, -9, -10)
      sut.path(4) must_== List(4, -8, -9, -10)
      sut.path(5) must_== List(5, 10)
      
      sut.freq.toList must_== List(5,7,10,15,20,45,12,22,35,57,102)
      sut.toGraph must_==
        """  102:10
          |   /   \_____________
          |  /                  \
          |45:5               57:9
          |            _______/  \___
          |           /              \
          |         22:7            35:8
          |         /  \__          /  \
          |        /      \      15:3  20:4
          |      10:2    12:6
          |              /  \
          |            5:0  7:1""".stripMargin
      ok
    }
    
    "render sassa_nf sample" in {
      val sut = new HuffmanTree(List(2,4,4,5,7,8))
      sut.toGraph must_==
        """           30:10
          |          __/  \__
          |         /        \
          |       13:8      17:9
          |     __/  \      /  \__
          |    /      \    /      \
          |  6:6     7:4  8:5    9:7
          |  /  \                /  \
          |2:0  4:1            4:2  5:3""".stripMargin
      ok
    }

    "process War And Peace" in {
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

      val counters = new mutable.HashMap[String, Int] withDefaultValue 0

      def isWord(s: String) = s.length > 1 && s.matches("^[a-z].*") && !Strings.isStop(s)
      
      def wordStream(source: Iterator[String]) = {
        for {
          line <- source
          l = Strings.normalize(line)
          word <- l.split(" ") filter isWord
        } yield {
          word.matches("^[a-z].*[a-z]") aka s"'$word' in \n<<$l>> from\n<<$line>>" must beTrue
          word
        }
      }
      
      source map extractNovel match {
        case Good(text) =>
          for (word <- wordStream(text)) counters(word) += 1
        case bad => failure(bad.listErrors.toString)
      }
      
      val cc = counters.toList.sortBy(_._2).zipWithIndex
      
      val dict: Map[String, Int] = {
        for {e: ((String, Int), Int) <- cc} yield e._1._1 -> e._2
      } toMap
      
      val freq: List[Int] = {
        for {e: ((String, Int), Int) <- cc} yield e._1._2
      } 
      
      val tree = new HuffmanTree(freq)
      tree.path(1000).length > 15 must beTrue
      tree.path(1000).length < 25 must beTrue
//      println(cc.size)
//      println(tree.path(1000))
//      println(tree.toGraph)
      
      failure(cc.toString)
      ok
    }
  }
}
