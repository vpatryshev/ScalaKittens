package scalakittens.experiments.penrose.turing

import scalakittens.experiments.penrose.turing.Tape._

import scala.language.postfixOps

class Tape(init: List[Γ]) {

  private class Stack {
    var data: List[Γ] = Nil

    def setTo(source: List[Γ]): Unit = data = source

    def @@ : Γ = data.headOption getOrElse void

    def !!(n: Γ): Unit =
      setTo(data match {
        case h :: t => n :: t
        case Nil => n :: Nil
      })

    def push(n: Γ): Unit = setTo(n :: data)

    def pop(): Γ = data match {
      case Nil => void
      case h :: t => setTo(t); h
    }
  }

  private val leftSide = new Stack
  private val rightSide = new Stack

  leftSide.setTo(Nil)
  rightSide.setTo(init)

  def read: Γ = rightSide @@

  def write(b: Γ): Unit = rightSide !! b

  def right(): Unit = leftSide.push(rightSide.pop())

  def left(): Unit = rightSide.push(leftSide.pop())

  override def equals(other: Any): Boolean = other match {
    case t: Tape => canonical == t.canonical
    case _ => false
  }

  private def canonical =
    all.mkString("")
      .replaceAll("0", " ")
      .trim
      .replaceAll(" ", "0") + "0"

  private def all: List[Γ] = leftSide.data.reverse ++ rightSide.data

  override def toString =
    s"${leftSide.data.reverse mkString ""}>${rightSide.data mkString ""}"
}

object Tape {
  type Γ = Int
  val void: Γ = 0

  def apply(src: String): Tape =
    new Tape(src.replaceAll(" ", "") map (_.toInt - '0') toList)
}