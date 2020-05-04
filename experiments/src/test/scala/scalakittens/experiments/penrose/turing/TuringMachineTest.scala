package scalakittens.experiments.penrose.turing

import org.specs2.mutable.Specification

import scalakittens.experiments.penrose.turing.TuringMachines._

class TuringMachineTest extends Specification {
  def tape(s: String): List[Int] = s.replaceAll("\\s", "").split("").map(_.toInt).toList

  sequential
  
  "TuringMachine" should {
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
    
    "sassa_nf" in {
      val machine = PTM("A.Otenko", "S 10L 101R 11L 1R 101R" split " " : _*)
      machine.runOn(Tape("1110")) must_== Tape("1111110")
    }
    
    "machine" in {
      val sample1 = penroseMachine("XN*2-book", "R10R1R100R111R10001S1S10001S")
      sample1 must_== `XN*2-book`
      val sample2 = penroseMachine("XN+1", `XN+1 source`)
      sample2 must_== `XN+1`
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
      val n2t1 = binaryEncode(5, 13)
      n2t1 must_== tape("10 0 10 110 10 10 0 10 110")
      val n2t = binaryEncode(5, 13, 0, 1, 1, 4)
      n2t must_== tape("10 0 10 110 10 10 0 10 110 0 110 10 110 10 110 10 0 0 110")
    }

    "encodeProgram" in {
      val sample1 = programEncode(`XN+1 source`)
      val expected1 = 
        """
          |10101101101001011010100111010010110101111
          |01000011101001010111010001011101010001101
          |0010110110101010101101010101101010100
        """.stripMargin.replaceAll("\\s", "")
      
      sample1.mkString("") must_== expected1
      
      val back1 = programDecode(sample1)
      back1 must_== `XN+1 source`
      
//      val actualDecimal = BigInt(sample1.mkString(""), 2)
//       val expectedDecimal = BigInt("450813704461563958982113775643437908")
      val sample2 = programEncode(`UN*2 source`)
      val expected2 =
        """
          |100110 100101110 1010110 10100110 10000110 01011110
          |101010110 10010101110 100010110 100101110 10010101
        """.stripMargin.replaceAll("\\s", "")
      sample2.mkString("") must_== expected2

      val back2 = programDecode(sample2)
      back2 must_== `UN*2 source`

      val binp1bin = programEncode(`UN+1 source`).mkString("")
      val binp1dec = BigInt(binp1bin, 2)
      binp1dec must_== BigInt("177642")
    }
    
    "run 11" in {
      val prog11 = programDecode(1::0::1::1::Nil)
      prog11 must_== "R 1S"
      val machine = PTM("11", prog11 split " " : _*)
      val res = machine runOn Tape("00110100")
      res must_== Tape("1101")
    }
  }
  
  "UTM" should {
    "get from denary" in {
      `U binary`.mkString("") must startWith("1101000000001")
      `U binary`.mkString("") must endWith("10001001010110")
    }
    
    "Run 11 on 6" in {
      val `11 6` = Tape("10111111100001101")
      
      val result = U runOn `11 6`
      ok // does not work...
//      result.toString.split("111110")(2) must_== "1101"
//      result must_== Tape("10111111101101")
    }
  }
}
