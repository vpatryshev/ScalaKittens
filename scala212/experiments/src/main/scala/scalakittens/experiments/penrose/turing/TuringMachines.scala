package scalakittens.experiments.penrose.turing

import scala.language.postfixOps

object TuringMachines {

  lazy val `UN+1`: Machine = Machine("UN+1",
    /* 0 */ "00R", "11R",
    /* 1 */ "01S", "11R"
  )
  
  val `UN+1 source` = "11R1S11R"

  lazy val EUC: Machine = Machine("Euclid",
    /* 0 */ "00R", "11L",
    /* 1 */ "101R", "11L",
    /* 2 */ "10100R", "110R",
    /* 3 */ "1000R", "111R",
    /* 4 */ "1000R", "1010R",
    /* 5 */ "1110L", "1101L",
    /* 6 */ "1100L", "11L",
    /* 7 */ "1110L", "10001L",
    /* 8 */ "10010L", "10001L",
    /* 9 */ "100R", "11L",
    /* 10 */ "00S", "10101R"
  )

  lazy val `UN*2`: Machine = Machine("UN*2",
    /* 0 */ "R", "10R",
    /* 1 */ "101L", "11R",
    /* 2 */ "110R", "1000R",
    /* 3 */ "01S", "111R",
    /* 4 */ "1011L", "1001R",
    /* 5 */ "101L", "1011L"
  )

  lazy val `XN+1`: Machine = Machine("XN+1",
    /* 0 */ "R", "11R",
    /* 1 */ "R", "101R",
    /* 2 */ "110L", "101R",
    /* 3 */ "1S", "1000L",
    /* 4 */ "1011L", "1001L",
    /* 5 */ "1100R", "101R",
    /* 6 */ "R", "1111R",
    /* 7 */ "111R", "1110R"
  )

  // note that 'R' in the beginning is missing, see page 68 of Penrose book 
  val `XN+1 source` = "11RR101R110L101R1S1000L1011L1001L1100R101RR1111R111R1110R"
  
  lazy val `XN*2`: Machine = Machine("XN*2",
    /* 0 */ "R", "11R",
    /* 1 */ "R", "101R",
    /* 2 */ "111L", "10001S",
    /* 3 */ "10000S", "1001L",
    /* 4 */ "10000S", "S"
  )

  lazy val `XN*2-book` = Machine("XN*2-book",
    /* 0 */ "R", "10R",
    /* 1 */ "1R", "100R",
    /* 2 */ "111R", "10001S",
    /* 3 */ "1S", "10001S"
  )
  
  def machine(name: String, program: String): Machine = {
    val RLS = Set('R', 'L', 'S')
    val commands = (List[String]() /: (program.reverse + 'R')) {
      case (cs, ch) if RLS(ch) => ""+ch::cs
      case (h::t, ch) => ch+h::t
    }
    
    Machine(name, commands.toArray:_*)
  }
  
  def unaryDecode(tape: List[Int]): List[Int] = {
    (List[Int]() /: tape) {
      case (n :: t, 1) => (n + 1) :: t
      case (xs, 0) => 0 :: xs
    } reverse
  }

  def binaryDecode(tape: List[Int]): List[Int] = {
    val chars = (List[Int]() /: tape) {
      case (Nil, 0) => 0 :: Nil
      case (n :: t, 0) => 0 :: n :: t
      case (0 :: t, 1) => 1 :: t
      case (1 :: t, 1) => 2 :: t
    } reverse

    (List[Int]() /: chars) {
      case (Nil, n) => n :: Nil
      case (h :: t, 0) => h * 2 :: t
      case (h :: t, 1) => h * 2 + 1 :: t
      case (h :: t, 2) => 0 :: h :: t
    }.tail.reverse
  }

  val binaryEncoding: Char => List[Int] = Map(
    '0' -> List(0),
    '1' -> List(1, 0),
    'R' -> List(1, 1, 0),
    'L' -> List(1, 1, 1, 0),
    'S' -> List(1, 1, 1, 1, 0),
    ',' -> List(1, 1, 0)) withDefaultValue Nil
  

  def binaryEncode(numbers: Int*): List[Int] = {

    for {
      n <- numbers.toList
      char <- n.toBinaryString + ","
      bit <- binaryEncoding(char)
    } yield bit
  }

  def encodeProgram(program: String): List[Int] = {
    val sp = if (program.endsWith("R")) program.dropRight(1) else program
    sp flatMap binaryEncoding toList
  }

  def unaryEncode(numbers: Int*): List[Int] = numbers.toList flatMap (n => 0 :: List.fill(n)(1))

  def main(args: Array[String]): Unit = {
//    println(`UN+1` run unaryEncode(4))
//    println(EUC run unaryEncode(4, 2))
//       println(`UN*2` run unaryEncode(4))
//    val xn2 = Machine("XN2",
//      /* 0 */ "00R", "10R",
//      /* 1 */ "01R", "100R",
//      /* 2 */ "111R", "10001S",
//      /* 3 */ "01S", "10001S"
//    )
//    xn2 run binaryEncode(11)
    val bits = encodeProgram(`XN+1 source`) mkString ""
    
    val n = BigInt(bits, 2)
    println(n)
  }
}






