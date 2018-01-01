package scalakittens.experiments.penrose.turing

import scala.language.postfixOps
import scalaz.Alpha.U

object TuringMachines {

  lazy val `UN+1`: Machine = Machine("UN+1",
    /* 0 */ "00R", "11R",
    /* 1 */ "01S", "11R"
  )

  val `UN+1 source` = "R11R1S11R"

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

  val `UN*2 source` = 
    "R 10R 101L 11R 110R 1000R 01S 111R 1011L 1001R 101L 1011L"
  
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

  val `XN+1 source` = 
    "R 11R R 101R 110L 101R 1S 1000L 1011L 1001L 1100R 101R R 1111R 111R 1110R"

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
    val commands = (List[String]() /: (program.replaceAll("\\s", "").reverse)) {
      case (cs, ch) if RLS(ch) => "" + ch :: cs
      case (h :: t, ch) => ch + h :: t
    }

    Machine(name, commands.toArray: _*)
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

  def programDecode(bits: List[Int]): String = {
    val programDecoding = Array("0", "1", "R ", "L ", "S ")
    val R = List(1,1,0)
    val cbs = R ++ bits ++ R

    val counts = (List[Int]() /: cbs) {
      case (Nil, b) => b :: Nil
      case (h :: t, 0) => 0 :: h :: t
      case (h :: t, 1) => h + 1 :: t
    }
    counts.tail.reverse map programDecoding mkString "" trim
  }

  def unaryEncode(numbers: Int*): List[Int] = numbers.toList flatMap (n => 0 :: List.fill(n)(1))

  val `U denary`: String =
    """
      |724485533533931757719839503961571123795236067255655963110814479 
      |6606505059404241090310483613632359365644443458382226883278767626556 
      |1446928141177150178425517075540856576897533463569424784885970469347 
      |2573998858228382779529468346052106116983594593879188554632644092552
      |5505820555989451890716537414896033096753020431553625034984529832320 
      |6515830476641421307088193297172341510569802627346864299218381721573 
      |3348282307345371342147505974034518437235959309064002432107734217885 
      |1492760797597634415123079586396354492269159479654614711345700145048 
      |1673375621725734645227310544829807849651269887889645697609066342044 
      |7798902191443793283001949357096392170390483327088259620130177372720 
      |2718625919914428275437422351355675134084222299889374410534305471044 
      |3686958764051781280194375308138706399427728231564252892375145654438 
      |9905278079324114482614235728619311833261065612275553181020751108533 
      |7633806031082361675045635852164214869542347187426437544428790062485 
      |8270912404220765387542644541334517485662915742999095026230097337381 
      |3772416217274772361020678685400289356608569682262014198248621698902 
      |6091309402985706001743006700868967590344734174127874255812015493663 
      |9389969058177385916540553567040928213322216314109787108145997866959 
      |9704509681841906299443656015145490488092208448003482249207730403043 
      |1884298993931352668823496621019471619107014619685231928474820344958 
      |9770955356110702758174873332729667899879847328409819076485127263100 
      |1740166787363477605857245036964434897992034489997455662402937487668 
      |8397514044516657077500605138839916688140725455446652220507242623923 
      |7921152531816251253630509317286314220040645713052758023076651833519 
      |95689139748137504926429605010013651980186945639498 
    """.stripMargin replaceAll("\\s", "")
  
  val `U binary` = "110" + BigInt(`U denary`).toString(2) + "110"

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
    val bits = programEncode(`XN+1 source`) mkString ""

    val n = BigInt(bits, 2)
    println(n)
  }
}
