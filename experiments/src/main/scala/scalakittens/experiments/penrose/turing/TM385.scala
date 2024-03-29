package scalakittens.experiments.penrose.turing

/**
 * A version of Turing Machine for COEN385
 *
 * Input is either 0 or 1; output is either 0 or 1;
 * L means moving tape left, R means tape moves right, L means tape moves left.
 * The whole transition step looks like this:
 * 1/0RA means if we see 1, write 0, move tape Right, new state will be A.
 *
 * Created by vpatryshev on 1/27/18.
 */
case class TM385(name: String, src: String) extends Machine(name) {

  val program: Map[(State, Int), (State, Int, Where)] = {
    val rows = src split " " grouped 3

    def compile(instr: String): (String, Int, Where) =
      (instr drop 2,
        instr.substring(0, 1).toInt,
        where(instr(1)))

    def entry(s: State, move: String): ((State, Int), (String, Int, Where)) = {
      val d = move.substring(0, 1)
      val target = move.split("/")(1)
      try {(s, d.toInt) → compile(target) } catch {
        case x: Exception => throw new IllegalArgumentException(move, x)
      }
    }

    val code = for {
      row ← rows.toList
      s = row(0)
      c0 = row(1)
      c1 = row(2)
    } yield entry(s, c0) :: entry(s, c1) :: Nil

    code.flatten.toMap
  }

  def decode(cmd: String): (State, Int, Where) = {
    val n = Integer.parseInt("0" + cmd.dropRight(1), 2)
    (String.valueOf(n / 2), n % 2, where(cmd.last))
  }

}

object TM385 {
  def test: Tape = {
    val plus1 = TM385("U+1", "0 0/0R0 1/1R1 1 1/1R1 0/1SS")
    plus1.run(List(0, 0, 1, 1, 1))

    val plus = TM385("U+", "0 0/0R0 1/0R1 1 1/1R1 0/1SS")
    plus.run(List(0, 0, 1, 1, 1, 0, 1, 1))
  }

  def Ortiz: Tape = {
    val minus = TM385("minus",
      "0 1/1r0 0/0rb b 1/1lc 0/0sS c 1/0rd 0/0lc d 0/0rd 1/0rb")
    minus.run("1111011")
  }

  def Yıldırım: Tape = {
    val minus = TM385("minus",
      "0 1/1lb 0/0l0 b 1/1lb 0/0lc c 0/0lc 1/0ld d 0/0re 1/1rf e 0/0re 1/0lh f 0/0rf 1/0rb h 0/0ss 1/1ss ")
    minus.run("11110111")
  }

  def Khodi: Tape = {
    val minus = TM385("minus",
      "0 0/0Lb 1/1lb b 1/1lb 0/0lc c 0/0lc 1/0ld d 0/0rf 1/1re e 0/0re 1/0rg f 0/0rf 1/0lh g 0/0lh 1/1lc h 0/0ss 1/1ss")
    minus.run("1111011")
  }

  def Sharma: Tape = {
    val plus = TM385("plus",
      "0 0/0R0 1/1Rb b 1/1Rb 0/1Rc c 1/1Rc 0/1Ld d 0/0Rd 1/0Re e 1/1ss 0/0ss"
    )
    plus.run("10111")
    //    val minus = TM385("minus",
    //      "0 0/0r0 1/0rb b 1/1rb 0/0lc c 0/1ss 1/0ld d 0/0rf 1/1re e 0/0re 1/0rg f 0/0rf 1/0lh g 0/0lh 1/1lc h 
    //      0/0ss 1/1ss")
    //    minus.run("1111011")
  }

  def Bo: Tape = {
    val minus = TM385("minus",
      "0 0/0ss 1/0lb b 0/0ss 1/1lc c 0/0ld 1/1lc d 0/0re 1/1ld e 0/0ss 1/0rf f 0/0ss 1/1rg g 0/0rh 1/1rg h 0/0l0 1/1rh"
    )
    minus.run("1101111")
  }

  def Nelson: Tape = {
    val plus = TM385("plus",
      "0 0/0ss 1/1rb b 1/1rb 1/1lc c 0/0ld 1/1lc d 0/0re 1/1ld e 0/0ss 1/0rf f 0/0ss 1/1rg g 0/0rh 1/1rg h 0/0l0 1/1rh"
    )
    plus.run("1101111")

    val minus = TM385("minus",
      "0 0/0ss 1/0lb b 0/0ss 1/1lc c 0/0ld 1/1lc d 0/0re 1/1ld e 0/0ss 1/0rf f 0/0ss 1/1rg g 0/0rh 1/1rg h 0/0l0 1/1rh"
    )
    minus.run("1101111")
  }

  def Martin: Tape = {

    val minus = TM385("minus",
      "0 0/0ss 1/0rb b 0/0rc 1/1rb c 0/0ld 1/1rc d 0/1ss 1/0le e 0/0lf 1/1le f 0/0r0 1/1lf"
    )
    minus.run("1101111")
  }

  def Mahajani: Tape = {
    val minus = TM385("minus",
      "0 0/0ss 1/0rb b 0/0rc 1/1rb c 0/0ld 1/1rc d 0/1ss 1/0le e 0/0lf 1/1le f 0/0r0 1/1lf"
    )
    minus.run("1101111")
  }

  def Jonathan: Tape = {
    val plus = TM385("plus", "0 0/1r1 1/1r0 1 0/0l2 1/1r1 2 1/0l3 0/0ss 3 0/0r4 1/1l3 4 0/0ss 1/1ss")
    plus.run("1101111")

    val minus = TM385("minus",
      "0 0/0s0 1/1r1 1 0/0r2 1/1r1 2 0/0r2 1/0l6 3 0/0l4 1/1l3 4 0/0l4 1/1r5 5 0/1r2 1/1r5 6 0/0l6 1/1r7 7 1/1s0 0/0s0")

    minus.run("1101111")
  }

  def Cassidy: Tape = {
    val plus = TM385("plus", "0 0/0ss 1/1Rb b 0/1Rc 1/1Rb c 1/1Rd 0/0ss d 0/0Le 1/1Rd e 0/0ss 1/0ss")
    plus.run("1101111")

    val minus = TM385("minus",
      "0 0/0ss 1/1Rb b 0/0Rc 1/1Rb c 0/0ss 1/1Rd d 0/0Lh 1/1Le e 0/0ss 1/0Lf f 0/0Lf 1/0Rg g 0/0Rg 1/1Rd h 1/0Li " +
        "0/0ss i 0/0Li 1/0ss")

    minus.run("1111011")
  }

  def Mose: Tape = {
    val plus = TM385("plus", "0 0/0ra 1/0ra a 0/0ss 1/0rb b 1/1rb 0/1ss")
    plus.run("1101111")

    val minus = TM385("minus",
      "0 0/0ra 1/0ra a 0/0ss 1/0rb b 0/0rc 1/1rb c 0/0ss 1/1rd d 0/0le 1/1rd e 0/0ss 1/0lf f 0/0ss 1/1lg g 0/0lh " +
        "1/1lg h 1/1lh 0/0li i 0/0ra 1/1li")

    minus.run("1101111")
  }

  def main(args: Array[String]): Unit = {
    Chen
    System.exit(0)
  }

  def Chen: Tape = {
    val minus = TM385("minus",
      "0 1/1l0 0/0lb b 1/0lc 0/0lb c 1/1rd 0/0re d 0/0rd 1/0lb e 0/0re 1/0lf f 0/0ss 1/1ss")
    minus.run("1111011")
  }

}