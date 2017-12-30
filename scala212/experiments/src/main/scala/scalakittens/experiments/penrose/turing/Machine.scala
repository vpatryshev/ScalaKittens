package scalakittens.experiments.penrose.turing

import language.postfixOps

case class Machine(name: String, src: (Int, String)*) {

  sealed trait Where
  case object L extends Where
  case object R extends Where
  case object S extends Where

  val where = Map('L' -> L, 'R' -> R, 'S' -> S)

  class Done extends Exception

  def decode(cmd: String): (Int, Int, Where) = {
    val next = cmd.dropRight(2).toInt
    val nw = cmd.takeRight(2)
    (next, nw.head.toInt - '0', where(nw.tail.head))
  }

  val program: Array[Array[(Int, Int, Where)]] = {
    val code = for {
      p <- 0 until src.size
      row = src(p)._2.split("/") map decode
    } yield row

    code.toArray
  }

  private var state = 0
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
    state = 0
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
