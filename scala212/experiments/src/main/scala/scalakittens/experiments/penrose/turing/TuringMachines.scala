package scalakittens.experiments.penrose.turing

import scala.language.postfixOps

object TuringMachines {

  val `UN+1`: Machine = Machine("UN+1", 
    0 -> "00R/11R",
    1 -> "01S/11R"
  )

  val EUC: Machine = Machine("Euclid",
    0 -> "00R/11L",
    1 -> "21R/11L",
    2 -> "100R/30R",
    3 -> "40R/31R",
    4 -> "40R/50R",
    5 -> "70L/61L",
    6 -> "60L/11L",
    7 -> "70L/81L",
    8 -> "90L/81L",
    9 -> "20R/11L",
    10 -> "00S/101R"
  )

  val `UN*2`: Machine = Machine("UN*2",
    0 -> "00R/10R",
    1 -> "21L/11R",
    2 -> "30R/40R",
    3 -> "01S/31R",
    4 -> "51L/41R",
    5 -> "21L/51L"
  )

  val `XN+1`: Machine = Machine("XN+1",
    0 -> "00R/11R",
    1 -> "00R/21R",
    2 -> "30L/21R",
    3 -> "01S/40L",
    4 -> "51L/41L",
    5 -> "60R/21R",
    6 -> "71R/71R",
    7 -> "31R/70R"
  )
  
  val `XN*2`: Machine = Machine("XN*2",
    0 -> "00R/11R",
    1 -> "00R/21R",
    2 -> "31L/81S",
    3 -> "80S/41L",
    4 -> "80S/00S"
  )

  val `XN*2-book` = Machine("XN*2-book",
    0 -> "00R/10R",
    1 -> "01R/20R",
    2 -> "31R/81S",
    3 -> "01S/81S"
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
    // println(EUC run unaryEncode(4, 2))
 //   println(`UN*2` run unaryEncode(4))
    val xn2 = Machine("XN2",
      0 -> "00R/10R",
      1 -> "01R/20R",
      2 -> "31R/81S",
      3 -> "01S/81S"
    )
    xn2 run binaryEncode(11)
  }
}






