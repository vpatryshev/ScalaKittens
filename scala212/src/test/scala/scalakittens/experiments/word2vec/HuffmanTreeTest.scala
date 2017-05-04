package scalakittens.experiments.word2vec

import org.specs2.mutable.Specification

class HuffmanTreeTest extends Specification {

  "HuffmanTree" should {

    "build a tree" in {
      // source: https://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html
      val sut = new HuffmanTree(5::7::10::15::20::45::Nil)
// this is an artistic graph representation
//    println(sut.toGraph)
      sut.parent must_== Array(6,6,7,8,8,10,7,9,9,10,-1)
      sut.freq.toList must_== List(5,7,10,15,20,45,12,22,35,57,102)      
      ok
    }
  }
}
