package ukkonen

/**
  * Created by vpatryshev on 1/30/17.
  */
class SuffixTreeTest extends org.specs2.mutable.Specification {
  def build(text: String): SuffixTree = {
    SuffixTree(text)
  }
  
  
  "SuffixTree" should {
    
    "single path" in {
      val sut: SuffixTree = build("ABCDEFGHIJKLMN$")
      sut.width must_== 15
      sut.contains("abc") must beFalse
      sut.contains("ABC") must beTrue
      sut.contains("CDE") must beTrue
      sut.contains("LMN") must beTrue
    }

    "dictionary" in {
      val sut = new SuffixTreeBuilder("GEEKSFORGEEKSS$".getBytes)
      val dict = sut.buildDictionary
      val expected = new Array[Byte](256)
      val FF: Byte = 255.toByte
      for (i <- expected.indices) expected(i) = FF
      for {
        (c, i) <- "ESGK$FOR".zipWithIndex 
      } expected(c.toByte) = i.toByte
      
      for (i <- 0 until 256) {
        dict(i) aka s"#$i '${i.toChar}' " must_== expected(i)
      }
      
      dict must_== expected
      
      val translated = sut.translate("GAGAE".getBytes)
      translated must_== Array[Byte](2,FF,2,FF,0)
    }
    
    "internally" in {
      val sut: SuffixTree = build("GEEKSFORGEEKS$")
      sut.width must_== 8
      val str = sut.Segment("GEEKS")
      val howMany = 1000
      val root: Node = sut.root
      val idx1 = 0
      val howTraversed = root.traverseEdge(str, idx1)
      howTraversed must_== root.PartialMatch
      val nextIdx1 = idx1 + root.edgeLength
      val c = str(nextIdx1)
      val c0 = root.child(c)

      c0 aka s"@$c" mustNotEqual null
      val c1 = root.child(c)
      c1 aka s"@$c" mustNotEqual DeadEnd
      val point1: Point = root.child(c)
      point1 mustNotEqual DeadEnd
      point1.isInstanceOf[Node] must beTrue
      point1.listAlignmentsAt(str, nextIdx1, howMany) must_== Set(MatchPoint(0,0), MatchPoint(0,8))
      root.PartialMatch.alignments(str, 0, howMany) must_== Set(MatchPoint(0,0), MatchPoint(0,8))
      root.listAlignmentsAt(str, 0, howMany) must_== Set(MatchPoint(0,0), MatchPoint(0,8))
      sut.listAlignmentsOfSegment(str, howMany) must_== MatchPoint(0,0)::MatchPoint(0,8)::Nil
    }
    
    "geeks" in {
      val sut: SuffixTree = build("GEEKSFORGEEKS$")
      sut.listAlignmentPoints("FOR") must_== 5::Nil
      sut.contains("FOR") must beTrue
      sut.listAlignmentPoints("GEEKS") must_== 0::8::Nil
      sut.contains("GEEK1") must beFalse
      sut.listAlignmentPoints("GEEKS1") must_== Nil
    }

    "ABBA" in {
      val sut = build("AABAACAADAABAAABAA$")

      sut.listAlignmentPoints("AA") must_== List(0, 3, 6, 9, 12, 13, 16)
      sut.contains("AA") must beTrue
      sut.contains("AAE") must beFalse
      sut.contains("?badbad") must beFalse
      sut.listAlignmentPoints("AAE") must_== Nil
      sut.listAlignmentPoints("AABA") must_== 0::9::13::Nil
      sut.contains("AABA") must beTrue
    }

    "AAA" in {
      val sut = build("AAAAAAAAA$")

      sut.listAlignmentPoints("AAAA") must_== List(0, 1, 2, 3, 4, 5)
      sut.contains("AAAA") must beTrue
      sut.contains("AA") must beTrue
      sut.listAlignmentPoints("AA") must_== List(0, 1, 2, 3, 4, 5, 6, 7)
      sut.contains("A") must beTrue
      sut.listAlignmentPoints("A") must_== List(0, 1, 2, 3, 4, 5, 6, 7, 8)
      sut.contains("AB") must beFalse
      sut.listAlignmentPoints("AB") must_== Nil
    }
    
    "biotech" in {
      val sut = SuffixTree("ACTGACTGTAGTAGTCAQ")
      sut.contains("A")
      sut.listAlignmentPoints("A") must_== 0::4::9::12::16::Nil
      sut.contains("TA")
      sut.listAlignmentPoints("TA") must_== 8::11::Nil
      sut.contains("GTA")
      sut.listAlignmentPoints("GTA") must_== 7::10::Nil
      sut.contains("ACTG")
      sut.listAlignmentPoints("ACTG") must_== 0::4::Nil
    }

    "partial segment" in {
      val sut = SuffixTree("ACTGACTGTAGTAGTCAQ")
      val bytes = "ACTGAGTA".getBytes

      sut.listSlidingAlignments(bytes, 6, 2) must_== Nil

      sut.listSlidingAlignments(bytes, 1, 4) must_== List(MatchPoint(0,0), MatchPoint(0,9), MatchPoint(0,12), MatchPoint(0,16))
      sut.listSlidingAlignments(bytes, 2, 5) must_== List(MatchPoint(0,0), MatchPoint(5,7), MatchPoint(0,4), MatchPoint(4,9), MatchPoint(4,12))
      sut.listSlidingAlignments(bytes, 2, 10) must_== List(MatchPoint(0,0), MatchPoint(5,7), MatchPoint(0,4), MatchPoint(4,9), MatchPoint(4,12))
      sut.listSlidingAlignments(bytes, 3, 3) must_== List(MatchPoint(0,0), MatchPoint(0,4), MatchPoint(4,9))
      sut.listSlidingAlignments(bytes, 3, 10) must_== List(MatchPoint(0,0), MatchPoint(5,7), MatchPoint(0,4), MatchPoint(4,9), MatchPoint(4,12))
      sut.listSlidingAlignments(bytes, 4, 1) must_== MatchPoint(0,0)::Nil

      sut.listSlidingAlignments(bytes, 4, 10) must_== List(MatchPoint(0,0), MatchPoint(0,4), MatchPoint(4,9))
    }
    
    "Bigger segment" in {
      val sut = SuffixTree("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGCTTCTGAACTG\nGTTACCTGCCGTGAGTAAATTAAAATTTTATTGACTTAGGTCACTAAATACTTTAACCAATATAGGCATAGCGCACAGAC\nAGATAAAAATTACAGAGTACACAACATCCATGAAACGCATTAGCACCACCATTACCACCACCATCACCATTACCACAGGT\nAACGGTGCGGGCTGACGCGTACAGGAAACACAGAAAAAAGCCCGCACCTGACAGTGCGGGCTTTTTTTTTCGACCAAAGG\nTAACGAGGTAACAACCATGCGAGTGTTGAAGTTCGGCGGTACATCAGTGGCAAATGCAGAACGTTTTCTGCGTGTTGCCG\nATATTCTGGAAAGCAATGCCAGGCAGGGGCAGGTGGCCACCGTCCTCTCTGCCCCCGCCAAAATCACCAACCACCTGGTG\nGCGATGATTGAAAAAACCATTAGCGGCCAGGATGCTTTACCCAATATCAGCGATGCCGAACGTATTTTTGCCGAACTTTT\nGACGGGACTCGCCGCCGCCCAGCCGGGGTTCCCGCTGGCGCAATTGAAAACTTTCGTCGATCAGGAATTTGCCCAAATAA\nAACATGTCCTGCATGGCATTAGTTTGTTGGGGCAGTGCCCGGATAGCATCAACGCTGCGCTGATTTGCCGTGGCGAGAAA\nATGTCGATCGCCATTATGGCCGGCGTATTAGAAGCGCGCGGTCACAACGTTACTGTTATCGATCCGGTCGAAAAACTGCT".replaceAll("[^\\w]", ""))
      
      val sample = "ATGTCGATCGCCATTATGGCCGGCGTATTAGAAGCGCGCG"
      val actual = sut.listAlignments(sample.getBytes)
      actual must_== MatchPoint(0, 720)::Nil
    }
  }
}
