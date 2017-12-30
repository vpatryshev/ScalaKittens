package scalakittens.experiments.penrose.turing

import language.postfixOps

case class Machine(name: String, initState: String, commands: (String, String)*) {

  sealed trait Where
  case object L extends Where
  case object R extends Where
  case object S extends Where

  val where = Map('L' -> L, 'R' -> R, 'S' -> S)

  class Done extends Exception
  
  def decode(cmd: String): (String, Int, Where) = {
    val next = cmd.dropRight(2)
    val nw = cmd.takeRight(2)
    (next, nw.head.toInt - '0', where(nw.tail.head))
  }

  val program: Map[String, Array[(String, Int, Where)]] = {

    val code = for {
      (label, command) <- commands
      cmd = command.replaceAll(" ", "").split(":").last
      row = cmd.split("/") map decode
    } yield label -> row
    code.toMap
  }

  private var state = initState
  private var tape: Tape = _

  def run(initTape: List[Int]): Tape = {
    init(initTape)
    println(s"Starting $name with $tape")
    try {
      while (true) step()
    } catch {
      case e: Done =>
    }

    tape
  }

  def init(initData: List[Int]): Unit = {
    state = initState
    tape = new Tape(initData)
  }

  def step(): Unit = try {
    val x = tape.read
    val (nextState, nextDigit, whither) = program(state)(x)
    tape.w(nextDigit)
    whither match {
      case L => tape.left()
      case R => tape.right()
      case S => throw new Done
    }

    state = nextState
  } finally {
    println(this)
  }

  override def toString = s"$state: $tape"
}
