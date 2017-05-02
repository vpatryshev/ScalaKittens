package scalakittens.ukkonen

/**
  * Created by vpatryshev on 1/30/17.
  */
class SuffixTreeTest extends org.specs2.mutable.Specification {
  def build(text: String): SuffixTree = {
    SuffixTree(text)
  }
  
  
  "SuffixTree" should {
    
    "behave with single path" in {
      val sut: SuffixTree = build("ABCDEFGHIJKLMN$")
      sut.checkForSubstring("abc") must beFalse
      sut.checkForSubstring("ABC") must beTrue
      sut.checkForSubstring("CDE") must beTrue
      sut.checkForSubstring("LMN") must beTrue
    }
    
    "behave with geeks" in {
      val sut: SuffixTree = build("GEEKSFORGEEKS$")
      sut.checkForSubstring("GEEKS") must beTrue
      sut.listAlignmentPoints("GEEKS") must_== 0::8::Nil
      sut.checkForSubstring("GEEK1") must beFalse
      sut.listAlignmentPoints("GEEKS1") must_== Nil
      sut.checkForSubstring("FOR") must beTrue
      sut.listAlignmentPoints("FOR") must_== 5::Nil
    }

    "behave with ABBA" in {
      val sut = build("AABAACAADAABAAABAA$")

      sut.checkForSubstring("AABA") must beTrue
      sut.listAlignmentPoints("AABA") must_== 0::9::13::Nil
      sut.checkForSubstring("AA") must beTrue
      sut.listAlignmentPoints("AA") must_== List(0, 3, 6, 9, 12, 13, 16)
      sut.checkForSubstring("AAE") must beFalse
      sut.listAlignmentPoints("AAE") must_== Nil
    }

    "behave with AAA" in {
      val sut = build("AAAAAAAAA$")

      sut.checkForSubstring("AAAA") must beTrue
      sut.listAlignmentPoints("AAAA") must_== List(0, 1, 2, 3, 4, 5)
      sut.checkForSubstring("AA") must beTrue
      sut.listAlignmentPoints("AA") must_== List(0, 1, 2, 3, 4, 5, 6, 7)
      sut.checkForSubstring("A") must beTrue
      sut.listAlignmentPoints("A") must_== List(0, 1, 2, 3, 4, 5, 6, 7, 8)
      sut.checkForSubstring("AB") must beFalse
      sut.listAlignmentPoints("AB") must_== Nil
    }
    
    "be a good biotech" in {
      val sut = SuffixTree("ACTGACTGTAGTAGTCAQ")
      sut.checkForSubstring("A")
      sut.listAlignmentPoints("A") must_== 0::4::9::12::16::Nil
      sut.checkForSubstring("TA")
      sut.listAlignmentPoints("TA") must_== 8::11::Nil
      sut.checkForSubstring("GTA")
      sut.listAlignmentPoints("GTA") must_== 7::10::Nil
      sut.checkForSubstring("ACTG")
      sut.listAlignmentPoints("ACTG") must_== 0::4::Nil
    }

    "use partial segment" in {
      val sut = SuffixTree("ACTGACTGTAGTAGTCAQ")
      val bytes = "ACTGAGTA".getBytes

      sut.listSlidingAlignments(bytes, 1, 4) must_== List(MatchPoint(0,0), MatchPoint(0,9), MatchPoint(0,12), MatchPoint(0,16))
      sut.listSlidingAlignments(bytes, 2, 5) must_== List(MatchPoint(0,0), MatchPoint(5,7), MatchPoint(0,4), MatchPoint(4,9), MatchPoint(4,12))
      sut.listSlidingAlignments(bytes, 2, 10) must_== List(MatchPoint(0,0), MatchPoint(5,7), MatchPoint(0,4), MatchPoint(4,9), MatchPoint(4,12))
      sut.listSlidingAlignments(bytes, 3, 3) must_== List(MatchPoint(0,0), MatchPoint(0,4), MatchPoint(4,9))
      sut.listSlidingAlignments(bytes, 3, 10) must_== List(MatchPoint(0,0), MatchPoint(5,7), MatchPoint(0,4), MatchPoint(4,9), MatchPoint(4,12))
      sut.listSlidingAlignments(bytes, 4, 1) must_== MatchPoint(0,0)::Nil
      sut.listSlidingAlignments(bytes, 6, 2) must_== Nil
      sut.listSlidingAlignments(bytes, 4, 10) must_== List(MatchPoint(0,0), MatchPoint(0,4), MatchPoint(4,9))
    }
  }
}
