package scalakittens.experiments.word2vec

/**
  * Shortcut for @see https://en.wikipedia.org/wiki/Sigmoid_function, aka σ, which is <code>1/(1+exp(-x))</code>
  * The problem is, exponentials are about 20x slower than using the table
  * You provide ε (result precision), we build the table (for the negative branch),
  * and the function σ returns the sigmoid value
  * 
  * Created by vpatryshev on 5/8/17 (I kind of remember this date).
  */

trait Sigma {
  // result precision
  def ε: Double
  require(ε > 0, s"ε=$ε must be positive")
  require(ε <= .125, s"ε=$ε must be below .125")
  // argument step
  val δ = ε * 2.5
  // max argument value
  val max = 2 - math.log(δ)

  require(max > δ, s"ε=$ε - too big, δ=$δ!")
  require(max / δ < 100000, s"ε=$ε - too small!")

  // table size
  val size = (max / δ + 2).toInt

  // table of half the function
  val table = {

    val t = for {
      i <- 0 until size
      x = i * δ
    } yield 1/(1 + math.exp(x))
  
    t.toArray
}
  
  def σ(x: Double): Double = {
    if (x < -max) 0                      else
    if (x >  max) 1                      else
    if (x < 0)    table((δ/2-x/δ).toInt) else 
                  1 - table((δ/2+x/δ).toInt)
  }
}

object Sigma extends Sigma {
  val ε = 0.01
}
