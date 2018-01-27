package scalakittens.experiments.penrose.turing

/**
  * Penrose Turing Machine, based on Penrose' book, "The Emperor's New Mind"
  * Created by vpatryshev on 1/27/18.
  */
case class PTM(name: String, src: String*) extends Machine(name) {

  def decode(cmd: String): (State, Int, Where) = {
    val n = Integer.parseInt("0" + cmd.dropRight(1), 2)
    (String.valueOf(n/2), n%2, where(cmd.last))
  }

  val program: Map[(State, Int), (State, Int, Where)] = {
    val code = for {
      p <- 0 until src.size / 2
      row = Array(src(2*p), src(2*p+1)) map decode
    } yield row

    val array = code.toArray
    val m = for {
      i <- array.indices
      row = array(i)
      j <- row.indices
      v = row(j)
    } yield (String.valueOf(i), j) -> v

    m.toMap
  }

}
