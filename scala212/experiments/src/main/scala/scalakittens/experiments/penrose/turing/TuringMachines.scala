package scalakittens.experiments.penrose.turing

import scala.language.postfixOps
import scalakittens.Strings

object TuringMachines {
  
  lazy val `UN+1`: Machine = PTM("UN+1",
    /* 0 */ "00R", "11R",
    /* 1 */ "01S", "11R"
  )

  val `UN+1 source` = "R11R1S11R"

  lazy val EUC: Machine = PTM("Euclid",
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

  lazy val `UN*2`: Machine = PTM("UN*2",
    /* 0 */ "R", "10R",
    /* 1 */ "101L", "11R",
    /* 2 */ "110R", "1000R",
    /* 3 */ "01S", "111R",
    /* 4 */ "1011L", "1001R",
    /* 5 */ "101L", "1011L"
  )

  val `UN*2 source` = 
    "R 10R 101L 11R 110R 1000R 01S 111R 1011L 1001R 101L 1011L"
  
  lazy val `XN+1`: Machine = PTM("XN+1",
    /* 0 */ "R", "11R",
    /* 1 */ "R", "101R",
    /* 2 */ "110L", "101R",
    /* 3 */ "1S", "1000L",
    /* 4 */ "1011L", "1001L",
    /* 5 */ "1100R", "101R",
    /* 6 */ "R", "1111R",
    /* 7 */ "111R", "1110R"
  )

  val `XN+1 source` = 
    "R 11R R 101R 110L 101R 1S 1000L 1011L 1001L 1100R 101R R 1111R 111R 1110R"

  lazy val `XN*2`: Machine = PTM("XN*2",
    /* 0 */ "R", "11R",
    /* 1 */ "R", "101R",
    /* 2 */ "111L", "10001S",
    /* 3 */ "10000S", "1001L",
    /* 4 */ "10000S", "S"
  )

  lazy val `XN*2-book` = PTM("XN*2-book",
    /* 0 */ "R", "10R",
    /* 1 */ "1R", "100R",
    /* 2 */ "111R", "10001S",
    /* 3 */ "1S", "10001S"
  )

  val RLS = Set('R', 'L', 'S')

  def penroseMachine(name: String, programSource: String): Machine = {
    val commands = (List[String]() /: (programSource.replaceAll("\\s", "").reverse)) {
      case (cs, ch) if RLS(ch) => "" + ch :: cs
      case (h :: t, ch) => ch + h :: t
    }

    PTM(name, commands.toArray: _*)
  }
  
  def unaryDecode(tape: List[Int]): List[Int] = {
    (List[Int]() /: tape) {
      case (n :: t, 1) => (n + 1) :: t
      case (xs, 0) => 0 :: xs
      case other => throw new IllegalArgumentException(tape.mkString("."))
    } reverse
  }

  def binaryDecode(tape: List[Int]): List[Int] = {
    val chars = (List[Int]() /: tape) {
      case (Nil, 0) => 0 :: Nil
      case (n :: t, 0) => 0 :: n :: t
      case (0 :: t, 1) => 1 :: t
      case (1 :: t, 1) => 2 :: t
      case other => throw new IllegalArgumentException(tape.mkString("."))
    } reverse

    (List[Int]() /: chars) {
      case (Nil, n) => n :: Nil
      case (h :: t, 0) => h * 2 :: t
      case (h :: t, 1) => h * 2 + 1 :: t
      case (h :: t, 2) => 0 :: h :: t
      case other => throw new IllegalArgumentException(tape.mkString("."))
    }.tail.reverse
  }

  private val binaryEncoding = Map(
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

  def programEncode(program: String): List[Int] = {
    val cp = program.tail/*omitting 'R'*/.replaceAll("\\s", "")
    (cp flatMap binaryEncoding toList) dropRight 3
  }

  def programDecode(bits: String): String = {
    programDecode("110" + BigInt(bits).toString(2) + "110" map(_ - '0') toList)
  }
   
  def programDecode(bits: List[Int]): String = {
    val programDecoding = Array("0", "1", "R ", "L ", "S ")
    val R = List(1,1,0)
    
    val cbs = if (bits.startsWith(R)) bits ++ R else R ++ bits ++ R

    val counts = (List[Int]() /: cbs) {
      case (Nil, b) => b :: Nil
      case (h :: t, 0) => 0 :: h :: t
      case (h :: t, 1) => h + 1 :: t
      case other => throw new IllegalArgumentException(other.toString)
    }
    counts.tail.reverse map programDecoding mkString "" trim
  }

  def unaryEncode(numbers: Int*): List[Int] = numbers.toList flatMap (n => 0 :: List.fill(n)(1))
  
  val `U Hexatrigesimal`: String =
    """
    |bntzcum2p6yt9etgzabg0xtdxnywjijb4zz6r7sk4gaysuygy736ivas
    |26wd6g81mr4fmctwwm3wpdxqtgdrpwjbcgwq47abvbca09qss8cleo7u
    |mth7tvfshvlz2iigt38533nd3hpa87y6zem1rxy2kgd7oh3xwsv1z91d
    |1yayfkae89k6uroq1fuwjggd6mv07rdbvxp5k3dbzri9pp7tf9bum3q3
    |4zqlxgv98rxr2m8vegyb0emgqxbl6zlxhnskysw34qbj0m17hawdhkbh
    |j6q5ee0pf1736u18diragpdrtt8nxse8zz060l6zpjg051ta6svgxtvc
    |2yci742wsc5zgx35q5g35jv9c255996avd4mlx92qs38zqilfs1bltys
    |tk2ejosrplaj85uem4uj2h8k13f697j3mmb1xzarsx8h35gs9a6glhxp
    |pdtjl0zq2a0oh6d4ugyfkqhy4g2slylky6b6utwfviek1e4dypvlf222
    |raxpblml44rmsm0d4d6ztjdtz36tknwfce4swyfm1wk0j7s1gwslc04y
    |o1zokvoi5lo54dk0oqmwk804jdf18jwmhqs621qa9jqc1r3c3aqq6jjh
    |uf98143tiu77bz05oe20sn8hkz0fbctmbwhvqcnz344iccdmrluf59vs
    |0rdepbhej7pdvjxfa9519j6sv3obfyxxl8w77ejf13kiyagss28drh46
    |z7zq02uonb4fcke2znbwsj91xsvyrt8yv8wzkqziifqcn7ccahoy98yf
    |jhzhcwdcinv4p7xyyl4unszzad9h7na4zrpjj4sxik4gm5tb5tfmvbwn
    |ofwkpq9qskhjjx16g18o27yjm4b7kxj7kzhvuoe0f31a1co8mxj55vwc
    |72y46dt09z5ec9lvynpmyqicwu1tqiz2tzl09mrgxx7m0gjqqa0dzmft
    |34ns8mg2xgk7mg2qwx5t3iam5q5my0jazo2upqb5edy8h9ajqwq3cqm2
    |o4lrhcvvokf9wvwqilyqrh1h17rykvjt5kf071ebpu2u09favm1c6dm""".stripMargin.replaceAll("\\s", "")

  private val ubs: String = "110" + BigInt(`U Hexatrigesimal`, 36).toString(2)
  
  val `U binary`: List[Int] = ubs + "110" map(_ - '0') toList
  
  val U: Machine = penroseMachine("UTM", programDecode(`U binary`))

  def main(args: Array[String]): Unit = {
        println(`UN+1` run unaryEncode(4))
    //    println(EUC run unaryEncode(4, 2))
    //       println(`UN*2` run unaryEncode(4))
    //    val xn2 = PenroseMachine("XN2",
    //      /* 0 */ "00R", "10R",
    //      /* 1 */ "01R", "100R",
    //      /* 2 */ "111R", "10001S",
    //      /* 3 */ "01S", "10001S"
    //    )
    //    xn2 run binaryEncode(11)

//    val T11 = "R 1S"
//    val T11bits = programEncode(T11)
//    println(T11bits)
//    val prog11 = programDecode(T11bits)
//    val machine = PenroseMachine("11", T11 split " " : _*)
//    val res = PenroseMachine runOn Tape("00110100")
//
//    println(res)

//    val `11 6` = Tape("00010111111101101000")
//
//    val result = U runOn `11 6`
//    println(result) // not a good result; it stops in front of the first 1 on the tape, moving it to the head... bad.

    //    val sassa_nf = PenroseMachine("sassa_nf", "S 10L 101R 11L 1R 101R" split " " : _*)
//    println(sassa_nf runOn Tape("1110"))
//    val bits = programEncode(`XN+1 source`) mkString ""
//
//    val n = BigInt(bits, 2)
//    println(n)
  }
}
