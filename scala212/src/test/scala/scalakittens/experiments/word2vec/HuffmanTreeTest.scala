package scalakittens.experiments.word2vec

import org.specs2.mutable.Specification

class HuffmanTreeTest extends Specification {

  "HuffmanTree" should {

    "build a tree" in {
      // source: https://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html
      val sut = new HuffmanTree(5::7::10::15::20::45::Nil)
// this is an artistic graph representation
      sut.parent must_== List(6,-6,7,8,-8,10,-7,9,-9,-10,0)
      sut.freq.toList must_== List(5,7,10,15,20,45,12,22,35,57,102)
      sut.toGraph must_==
        """    102:10
          |    /    \
          |   /      ----------------
          |  /                       \
          |45:5                    57:9
          |                        /  \
          |            ------------    ---
          |           /                   \
          |          22:7                35:8
          |          /  \                /  \
          |      10:2     12:6       15:3  20:4
          |               / \
          |            5:0  7:1""".stripMargin
      ok
    }
  }
}
