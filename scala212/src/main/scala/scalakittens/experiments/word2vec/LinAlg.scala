package scalakittens.experiments.word2vec

/**
  * Linear Algebra ops (typeclass?), Scala version
  * Created by vpatryshev on 5/7/17.
  */
object LinAlg {
  
  implicit class Vec(val data: Array[Double]) {
    val dim: Int = data.length

    def *(other: Vec): Double = {
      require(dim == other.dim)
      (data zip other.data) map {case (a,b) => a*b} sum
    }
    
    def *(scalar: Double): Vec = new Vec(data map (scalar *))

    def +(other: Vec): Vec = {
      require(dim == other.dim)
      new Vec((data zip other.data) map {case (a,b) => a+b})
    }

    def -(other: Vec): Vec = {
      require(dim == other.dim)
      new Vec((data zip other.data) map {case (a,b) => a-b})
    }

    def *=(scalar: Double): Vec = {
      for (i <- data.indices) data(i) *= scalar
      this
    }
    
    def +=(other: Vec): Vec = {
      require(dim == other.dim)
      for (i <- data.indices) data(i) += other.data(i)
      this
    }

    def -=(other: Vec): Vec = {
      require(dim == other.dim)
      for (i <- data.indices) data(i) -= other.data(i)
      this
    }

    /**
      * Nudge this vector in the direction of other vector, with a coefficient.
      * Modifies in place.
      * @param other other vector
      * @param coeff coefficietn
      * @return this + coeff * other
      */
    def nudge(other: Vec, coeff: Double): Vec = {
      require(dim == other.dim)
      for (i <- data.indices) data(i) += coeff * other.data(i)
      this
    }
  }
}
