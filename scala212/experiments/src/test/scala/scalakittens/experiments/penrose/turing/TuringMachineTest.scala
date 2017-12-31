package scalakittens.experiments.penrose.turing

import org.specs2.mutable.Specification

import scalakittens.experiments.penrose.turing.TuringMachines._

class TuringMachineTest extends Specification {
  def tape(s: String): List[Int] = s.replaceAll(" ", "").split("").map(_.toInt).toList

  sequential
  
  "TuringMachineTest" should {
    "EUC" in {
      val res = EUC run unaryEncode(4, 2)
      res must_== Tape("11")
    }

    "UN+1" in {
      val res = `UN+1` run unaryEncode(4)
      res must_== Tape("00111110")
    }

    "UNx2" in {
      val res = `UN*2` run unaryEncode(5)
      val expected = new Tape(unaryEncode(10))
      res must_== expected
    }

    "XN+1" in {
      def plus1(n: Int) = {
        `XN+1` run binaryEncode(n)
      }
      for {
        i <- 0 to 256
      } plus1(i) must_== new Tape(binaryEncode(i+1))
      
      ok
    }

    "XN*2" in {
      def double(n: Int) = {
        `XN*2` run binaryEncode(n)
      }
      double(1) must_== new Tape(binaryEncode(2))

      for {
        i <- 0 to 256
      } double(i) must_== new Tape(binaryEncode(i*2))

      double(7) must_== new Tape(binaryEncode(14))
      println("?")
      ok
    }

    "XN*2-book" in {
      def double(n: Int) = {
        `XN*2-book` run binaryEncode(n)
      }
      double(1) must_== new Tape(binaryEncode(2))

      for {
        i <- 0 to 256
      } double(i) must_== new Tape(binaryEncode(i*2))

      double(7) must_== new Tape(binaryEncode(14))
      println("?")
      ok
    }
    
    "machine" in {
      val actual = machine("XN*2-book", "R10R1R100R111R10001S1S10001S")
      actual must_== `XN*2-book`
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
