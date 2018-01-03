package scalakittens.experiments.penrose.turing

import language.postfixOps

case class Machine(name: String, src: String*) {

  sealed trait Where
  case object L extends Where
  case object R extends Where
  case object S extends Where

  val where = Map('L' -> L, 'R' -> R, 'S' -> S)

  class Done extends Exception

  def decode(cmd: String): (Int, Int, Where) = {
    val n = Integer.parseInt("0" + cmd.dropRight(1), 2)
    (n/2, n%2, where(cmd.last))
  }

  val program: Array[Array[(Int, Int, Where)]] = {
    val code = for {
      p <- 0 until src.size / 2
      row = Array(src(2*p), src(2*p+1)) map decode
    } yield row

    code.toArray
  }

  private var state = 0
  private var tape: Tape = _

  def run(initData: List[Int]): Tape = runOn(new Tape(initData))

  def runOn(t: Tape): Tape = {
    tape = t
    state = 0
    println(s"Starting $name with $tape")
    try {
      while (true) step()
    } catch {
      case e: Done =>
    }

    tape
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
    dumpState()
  }

  override def toString: String = program.map(_.mkString("/")).mkString(";")
  
  override def equals(o: Any): Boolean = o match {
    case other: Machine => toString == other.toString
    case _ => false
  }
  
  def dumpState(): Unit = println(s"$state: $tape")
}
