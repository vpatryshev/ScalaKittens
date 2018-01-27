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

  def decode(cmd: String): (State, Int, Where) = {
    val n = Integer.parseInt("0" + cmd.dropRight(1), 2)
    (String.valueOf(n/2), n%2, where(cmd.last))
  }

  val program: Map[(State, Int), (State, Int, Where)] = {
    val rows = src split " " grouped(3)

    def entry(s: String, move: String) = {
      val target = move.split("/")(1)
      (s.asInstanceOf[State], move.substring(0, 1).toInt) -> 
        (target.drop(2).asInstanceOf[State],
         target.substring(0, 1).toInt,
         where(target(1)))
    }
    
    val code = for {
      row <- rows.toList
      s = row(0)
      c0 = row(1)
      c1 = row(2)
    } yield entry(s, c0)::entry(s, c1)::Nil

    code.flatten.toMap
  }

}

object TM385 {
  def main(args: Array[String]): Unit = {
    val plus1 = TM385("U+1", "0 0/0R0 1/1R1 1 1/1R1 0/1SS")
    plus1.run(List(0, 0, 1,1,1))
    
    val plus = TM385("U+", "0 0/0R0 1/0R1 1 1/1R1 0/1SS")
    plus.run(List(0, 0, 1,1,1, 0, 1,1))
  }
  
}