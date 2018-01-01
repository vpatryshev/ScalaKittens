package scalakittens.experiments.penrose.turing

import language.postfixOps

class Tape(init: List[Int]) {

  class Stack {
    var data: List[Int] = Nil

    def setTo(source: List[Int]): Unit = data = source

    def @@ : Int = data.headOption getOrElse 0

    def !!(n: Int): Unit =
      setTo(data match {
        case h :: t => n :: t
        case Nil => n :: Nil
      })

    def push(n: Int): Unit = setTo(n :: data)

    def pop(): Int = data match {
      case Nil => 0
      case h :: t => setTo(t); h
    }
  }

  val leftSide = new Stack
  val rightSide = new Stack

  def this(src: String) = this(src map (_.toInt - '0') toList)

  leftSide.setTo(Nil)
  rightSide.setTo(init)

  def read: Int = rightSide @@

  def w(b: Int): Unit = rightSide !! b

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

  def all: List[Int] = leftSide.data.reverse ++ rightSide.data

  override def toString =
    s"${leftSide.data.reverse mkString ""} ${rightSide.data mkString ""}"
}

object Tape {
  def apply(src: String): Tape = 
    new Tape(src.replaceAll(" ", "") map (_.toInt - '0') toList)
}