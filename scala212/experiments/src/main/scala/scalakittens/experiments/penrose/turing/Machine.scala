package scalakittens.experiments.penrose.turing

import language.postfixOps
import scalakittens.experiments.penrose.turing.Tape.Γ
import scalaz.StreamT.Done

 abstract class Machine(name: String) { self =>
   
  sealed trait Where
  case object L extends Where
  case object R extends Where
  case object S extends Where

  val where = Map('L' -> L, 'R' -> R, 'S' -> S)

  type State = String
  val initState: State = "0"
  
  class Done extends Exception

  val program: Map[(State, Int), (State, Int, Where)]
   
  private var state = initState
  private var tape: Tape = _

  def run(initData: List[Int]): Tape = runOn(new Tape(initData))

  def runOn(t: Tape): Tape = {
    tape = t
    state = initState
    println(s"Starting $self with $tape")
    try {
      while (true) step()
    } catch {
      case e: Done =>
    }

    tape
  }

  def step(): Unit = try {
    val x = tape.read
    val (nextState, nextDigit, whither) = program((state, x))
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

  override def toString: String = s"$name:\n$program"
  
  override def equals(o: Any): Boolean = o match {
    case other: Machine => toString == other.toString
    case _ => false
  }
  
  def dumpState(): Unit = println(s"$state: $tape")
}
