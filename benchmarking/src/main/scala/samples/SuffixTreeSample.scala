package samples

import anuragsingh.{SuffixTree => STA}
import org.openjdk.jmh.annotations.Benchmark
import ukkonen.{SuffixTree => STU}

/**
  * Created by vpatryshev on 1/30/17.
  */
class SuffixTreeSample {
  def buildA(text: String): STA = {
    STA(text)
  }

  def buildU(text: String): STU = {
    STU(text)
  }

  @Benchmark def `behave_with_single_path.A`() {
    val sut = buildA("ABCDEFGHIJKLMN$")
    sut.checkForSubstring("abc")
    sut.checkForSubstring("ABC")
    sut.checkForSubstring("CDE")
    sut.checkForSubstring("LMN")
  }

  @Benchmark def `behave_with_geeks.A`() {

    val sut = buildA("GEEKSFORGEEKS$")
    sut.checkForSubstring("GEEKS")
    sut.listAlignmentPoints("GEEKS")
    sut.checkForSubstring("GEEK1")
    sut.listAlignmentPoints("GEEKS1")
    sut.checkForSubstring("FOR")
    sut.listAlignmentPoints("FOR")
  }

  @Benchmark def `behave_with_ABBA.A`() {
    val sut = buildA("AABAACAADAABAAABAA$")

    sut.checkForSubstring("AABA")
    sut.listAlignmentPoints("AABA")
    sut.checkForSubstring("AA")
    sut.listAlignmentPoints("AA")
    sut.checkForSubstring("AAE")
    sut.listAlignmentPoints("AAE")
  }

  @Benchmark def `behave_with_AAA.A`() {
    val sut = buildA("AAAAAAAAA$")

    sut.checkForSubstring("AAAA")
    sut.listAlignmentPoints("AAAA")
    sut.checkForSubstring("AA")
    sut.listAlignmentPoints("AA")
    sut.checkForSubstring("A")
    sut.listAlignmentPoints("A")
    sut.checkForSubstring("AB")
    sut.listAlignmentPoints("AB")
  }

  @Benchmark def `be_a_good_biotech.A`() {
    val sut = STA("ACTGACTGTAGTAGTCAQ")
    sut.checkForSubstring("A")
    sut.listAlignmentPoints("A")
    sut.checkForSubstring("TA")
    sut.listAlignmentPoints("TA")
    sut.checkForSubstring("GTA")
    sut.listAlignmentPoints("GTA")
    sut.checkForSubstring("ACTG")
    sut.listAlignmentPoints("ACTG")
  }

  @Benchmark def `use_partial_segment.A`() {
    val sut = STA("ACTGACTGTAGTAGTCAQ")
    val bytes = "ACTGAGTA".getBytes

    sut.listSlidingAlignments(bytes, 1, 4)
    sut.listSlidingAlignments(bytes, 2, 5)
    sut.listSlidingAlignments(bytes, 2, 10)
    sut.listSlidingAlignments(bytes, 3, 3)
    sut.listSlidingAlignments(bytes, 3, 10)
    sut.listSlidingAlignments(bytes, 4, 1)
    sut.listSlidingAlignments(bytes, 6, 2)
    sut.listSlidingAlignments(bytes, 4, 10)
  }

  @Benchmark def `behave_with_single_path.U`() {
    val sut = buildU("ABCDEFGHIJKLMN$")
    sut.checkForSubstring("abc")
    sut.checkForSubstring("ABC")
    sut.checkForSubstring("CDE")
    sut.checkForSubstring("LMN")
  }

  @Benchmark def `behave_with_geeks.U`() {
    val sut = buildU("GEEKSFORGEEKS$")
    sut.checkForSubstring("GEEKS")
    sut.listAlignmentPoints("GEEKS")
    sut.checkForSubstring("GEEK1")
    sut.listAlignmentPoints("GEEKS1")
    sut.checkForSubstring("FOR")
    sut.listAlignmentPoints("FOR")
  }

  @Benchmark def `behave_with_ABBA.U`() {
    val sut = buildU("AABAACAADAABAAABAA$")

    sut.checkForSubstring("AABA")
    sut.listAlignmentPoints("AABA")
    sut.checkForSubstring("AA")
    sut.listAlignmentPoints("AA")
    sut.checkForSubstring("AAE")
    sut.listAlignmentPoints("AAE")
  }

  @Benchmark def `behave_with_AAA.U`() {
    val sut = buildU("AAAAAAAAA$")

    sut.checkForSubstring("AAAA")
    sut.listAlignmentPoints("AAAA")
    sut.checkForSubstring("AA")
    sut.listAlignmentPoints("AA")
    sut.checkForSubstring("A")
    sut.listAlignmentPoints("A")
    sut.checkForSubstring("AB")
    sut.listAlignmentPoints("AB")
  }

  @Benchmark def `be_a_good_biotech.U`() {
    val sut = STU("ACTGACTGTAGTAGTCAQ")
    sut.checkForSubstring("A")
    sut.listAlignmentPoints("A")
    sut.checkForSubstring("TA")
    sut.listAlignmentPoints("TA")
    sut.checkForSubstring("GTA")
    sut.listAlignmentPoints("GTA")
    sut.checkForSubstring("ACTG")
    sut.listAlignmentPoints("ACTG")
  }

  @Benchmark def `use_partial_segment.U`() {
    val sut = STU("ACTGACTGTAGTAGTCAQ")
    val bytes = "ACTGAGTA".getBytes

    sut.listSlidingAlignments(bytes, 1, 4)
    sut.listSlidingAlignments(bytes, 2, 5)
    sut.listSlidingAlignments(bytes, 2, 10)
    sut.listSlidingAlignments(bytes, 3, 3)
    sut.listSlidingAlignments(bytes, 3, 10)
    sut.listSlidingAlignments(bytes, 4, 1)
    sut.listSlidingAlignments(bytes, 6, 2)
    sut.listSlidingAlignments(bytes, 4, 10)
  }
}
