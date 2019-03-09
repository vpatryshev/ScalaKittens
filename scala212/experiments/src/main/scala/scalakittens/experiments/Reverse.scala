package scalakittens.experiments

import scala.annotation.tailrec

object Reverse {
  
  def recurse[X](list: List[X]): List[X] = {
    @tailrec def doit(from: List[X], to: List[X]): List[X] =
      from match {
        case Nil => to
        case x::xs => doit(xs, x::to)
      }

    doit(list, Nil)
  }
  
  def tonymorris[X](list: List[X]): List[X] = (List[X]() /: list) { 
    case (xs, x) => x::xs
  }
  
  case class Var[X](var x: X)
  
  def snowps[X](list: List[Var[X]]): List[Var[X]] = {
    val n = list.length
    for {
      i <- 0 until list.length / 2
    } {
      val xi = list(i).x
      val j = n - i - 1
      list(i).x = list(j).x
      list(j).x = xi
    }
    
    list
  }

  def snowpsA[X](list: Array[X]): Array[X] = {
    val n = list.length
    for {
      i <- 0 until list.length / 2
    } {
      val xi = list(i)
      val j = n - i - 1
      list(i) = list(j)
      list(j) = xi
    }

    list
  }
  
  def time(what: String)(op: => Any): Unit = {
    val t0 = System.currentTimeMillis
    op
    val t1 = System.currentTimeMillis
    val dt = t1 - t0
    println(s"$what: $dt ms")
  }
  
  def main(args: Array[String]): Unit = {
    val n = 70000
    val sut = (for { i <- 0 until n} yield s"<<$i>>") toList
    val sut1 = sut map (Var(_))
    val sut2 = sut.toArray
    println(s"Running $n elements")
    time("Recursively")(recurse(sut1))
    time("FlipCons")(tonymorris(sut1))
    time("Snowps")(snowps(sut1))
    time("Snowps with array")(snowpsA(sut2))
  }
}
