package scalakittens.experiments.word2vec

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

      val scanner = TextScanner.WarAndPeace
      
      def source() = IO.linesFromResource("/warandpeace.txt")

      source map scanner.scan match {
        case Good((index, words, freqs)) =>

          val tree = new HuffmanTree(freqs)
          tree.path(1000).length > 15 must beTrue
          tree.path(1000).length < 25 must beTrue
        //      println(cc.size)
        //      println(tree.path(1000))
        //      println(tree.toGraph)

        //      failure(cc.toString)
        case bad => failure(bad.listErrors.toString)
      }
      
      ok
    }
  }
}
