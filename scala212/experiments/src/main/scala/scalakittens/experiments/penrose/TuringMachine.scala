package scalakittens.experiments.penrose

import scala.language.postfixOps

object TuringMachine {

  val where = Map('L' -> L, 'R' -> R, 'S' -> S)
  val `UN+1`: TM = TM("UN+1", 0 -> "00R/11R", 1 -> "01S/11R")
  
  val EUC: TM = TM("Euclid",
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
  
  val `UNx2`: TM = TM("UNx2",
    0 -> "00R/10R",
    1 -> "21L/11R",
    2 -> "30R/40R",
    3 -> "01S/31R",
    4 -> "51L/41R",
    5 -> "21L/51L"
  )
  
  val `XN+1`: TM = TM("XN+1",
    0 -> "00R/11R",
    1 -> "00R/21R",
    2 -> "30L/21R",
    3 -> "01S/40L",
    4 -> "51L/41L",
    5 -> "60R/21R",
    6 -> "71R/71R",
    7 -> "31R/70R"
  )

  def Tape(src: String): Tape = Tape(src.replaceAll(" ", "") map (_.toInt - '0') toList)

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

  def main(args: Array[String]): Unit = {
    //    println(`UN+1` run unaryEncode(4))
    // println(EUC run unaryEncode(4, 2))
    println(`UNx2` run unaryEncode(4))
  }

  def unaryEncode(numbers: Int*): List[Int] = numbers.toList flatMap (n => 0 :: List.fill(n)(1))

  sealed trait Where

  case object L extends Where

  case object R extends Where

  case object S extends Where

  class Stack {
    var data: List[Int] = Nil

    def setTo(source: List[Int]): Unit = {
      data = source ++ Nil
    }

    def @@ : Int = data.headOption getOrElse 0

    def !!(n: Int): Unit = {
      data = data match {
        case h :: t => n :: t
        case Nil => n :: Nil
      }
    }

    def push(n: Int): Unit = {
      data = n :: data
    }

    def pop(): Int = data match {
      case Nil => 0
      case h :: t => data = t; h
    }
  }

  case class Tape(init: List[Int]) {
    val left = new Stack
    val right = new Stack

    def this(src: String) = this(src map (_.toInt - '0') toList)

    left.setTo(Nil)
    right.setTo(init)

    def read: Int = right @@

    def w(b: Int): Unit = right !! b

    def move(where: Where): Unit = where match {
      case L => right.push(left.pop())
      case R => left.push(right.pop())
      case S => throw new Done
    }

    override def equals(other: Any): Boolean = other match {
      case t: Tape => canonical == t.canonical
      case _ => false

    }

    private def canonical =
      all.mkString("")
        .replaceAll("0", " ")
        .trim
        .replaceAll(" ", "0") + "0"

    def all: List[Int] = left.data.reverse ++ right.data

    override def toString =
      s"${left.data.reverse mkString ""} ${right.data mkString ""}"
  }

  class Done extends Exception

  case class TM(name: String, src: (Int, String)*) {
    val program: Array[Array[(Int, Int, Where)]] = {
      val m = src.toMap mapValues (_.split("/"))
      val memMax = m.keys max

      def decode(cmd: String): (Int, Int, Where) = {
        val next = cmd.dropRight(2).toInt
        val nw = cmd.takeRight(2)
        (next, nw.head.toInt - '0', where(nw.tail.head))
      }

      val code = for {
        p <- 0 to memMax
        row = m(p) map decode
      } yield row

      code.toArray
    }

    private var state = 0
    private var tape: Tape = _

    def run(initTape: List[Int]): Tape = {
      init(initTape)
      try {
        while (true) step()
      } catch {
        case e: Done =>
      }

      tape
    }

    def init(initData: List[Int]): Unit = {
      state = 0
      tape = Tape(initData)
    }

    def step(): Unit = try {
      val x = tape.read
      val (nextState, nextDigit, whither) = program(state)(x)
      tape.w(nextDigit)
      tape.move(whither)
      state = nextState
    } finally {
      println(this)
    }

    override def toString = s"$state: $tape"
  }

}
