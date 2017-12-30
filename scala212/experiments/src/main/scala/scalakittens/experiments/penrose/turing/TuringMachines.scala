package scalakittens.experiments.penrose.turing

import scala.language.postfixOps

object TuringMachines {

  val `UN+1`: Machine = Machine("UN+1",
    "zeroes",   
    "zeroes" -> "zeroes 0 R / ones 1 R",
    "ones" -> "zeroes 1 S / ones 1 R"
  )

  val EUC: Machine = Machine("Euclid",
    "start",
    "start" -> "start 0R / atFirst 1L",
    "atFirst" -> "beforeFirst 1R / atFirst 1L",
    "beforeFirst" -> "done 0R / skipFirst 0R",
    "skipFirst" -> "separator 0R / skipFirst 1R",
    "separator" -> "separator 0R / atSecond 0R",
    "atSecond" -> "doneSubtraction 0L / backToFirst 1L",
    "backToFirst" -> "backToFirst 0L / atFirst 1L",
    "doneSubtraction" -> "doneSubtraction 0L / skipSecond 1L",
    "skipSecond" -> "beforeSecond 0L / skipSecond 1L",
    "beforeSecond" -> "beforeFirst 0R / atFirst 1L",
    "done" -> "done 0S / done 1R"
  )

  val `UN*2`: Machine = Machine("UN*2", "start",
    "start" -> "start 0R / passOne 0R",
    "passOne" -> "endOfN 1L / passOne 1R",
    "endOfN" -> "done 0R / add1To2N 0R",
    "done" -> "done 1S / done 1R",
    "add1To2N" -> "backToN 1L / add1To2N 1R",
    "backToN" -> "endOfN 1L/ backToN 1L"
  )

  val `XN+1`: Machine = Machine("XN+1", "0",
    "0" -> "00R/11R",
    "1" -> "00R/21R",
    "2" -> "30L/21R",
    "3" -> "01S/40L",
    "4" -> "51L/41L",
    "5" -> "60R/21R",
    "6" -> "71R/71R",
    "7" -> "31R/70R"
  )
  
  val `XN*2`: Machine = Machine("XN*2", "0",
    "0" -> "00R/11R",
    "1" -> "00R/21R",
    "2" -> "31L/81S",
    "3" -> "80S/41L",
    "4" -> "80S/00S"
  )

  val `XN*2-book` = Machine("XN*2-book", "0",
    "0" -> "00R/10R",
    "1" -> "01R/20R",
    "2" -> "31R/81S",
    "3" -> "01S/81S"
  )

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

  def binaryEncode(numbers: Int*): List[Int] = {
    def encodeChar = Map(
      '0' -> List(0),
      '1' -> List(1, 0),
      ',' -> List(1, 1, 0)) withDefaultValue Nil

    for {
      n <- numbers.toList
      char <- n.toBinaryString + List(",")
      bit <- encodeChar(char)
    } yield bit
  }

  def unaryEncode(numbers: Int*): List[Int] = numbers.toList flatMap (n => 0 :: List.fill(n)(1))

  def main(args: Array[String]): Unit = {
    //    println(`UN+1` run unaryEncode(4))
//    println(EUC run unaryEncode(4, 2))
    println(`UN*2` run unaryEncode(4) ++ (0::Nil))
//    val xn2 = Machine("XN2", "0",
//      "0" -> "00R/10R",
//      "1" -> "01R/20R",
//      "2" -> "31R/81S",
//      "3" -> "01S/81S"
//    )
//    xn2 run binaryEncode(11)
  }
}






