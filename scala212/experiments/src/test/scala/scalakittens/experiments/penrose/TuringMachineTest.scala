package scalakittens.experiments.penrose

import org.specs2.mutable.Specification

import scalakittens.experiments.penrose.TuringMachine._

class TuringMachineTest extends Specification {
  def tape(s: String): List[Int] = s.replaceAll(" ", "").split("").map(_.toInt).toList

  "TuringMachineTest" should {
    "EUC" in {
      val res = EUC run unaryEncode(4, 2)
      res must_== Tape("11")
    }

    "UN+1" in {
      val res = `UN+1` run unaryEncode(4)
      res must_== Tape("00111110")
    }

    "XN+1" in {
      def plus1(n: Int) = `XN+1` run binaryEncode(n)
      for {
        i <- 0 to 256
      } plus1(i) must_== Tape(binaryEncode(i+1))
      
      plus1(255) must_== Tape(binaryEncode(256))
      println("?")
      ok
    }
    
    "UNx2" in {
      val res = `UNx2` run unaryEncode(5)
      val expected = Tape("0001111111111")
      res must_== expected
    }
    
    "unaryDecode" in {
      val t = tape("010 0 010110101011010 0 01110101011110 0110")
      val nums = List(1, 0, 0, 1, 2, 1, 1, 2, 1, 0, 0, 3, 1, 1, 4, 0, 2, 0)
      val t2n = unaryDecode(t)
      t2n must_== nums
    }

    "unaryEncode" in {
      val t = tape("010 0 010110101011010 0 01110101011110 0110")
      unaryEncode(1, 0, 0, 1, 2, 1, 1, 2, 1, 0, 0, 3, 1, 1, 4, 0, 2, 0) must_== t
    }

    "binaryDecode" in {
      val t = tape("0000 10 0 10 110 10 10 0 10 110 0 110 10 110 110 110 10 110 10 0 0 110 00")
      val nums = List(5, 13, 0, 1, 0, 0, 1, 4)
      val t2n = binaryDecode(t)
      t2n must_== nums
    }

    "binaryEncode" in {
      val n2t = binaryEncode(5, 13, 0, 1, 1, 4)
      n2t must_== tape("10 0 10 110 10 10 0 10 110 0 110 10 110 10 110 10 0 0 110")
    }

  }
}
