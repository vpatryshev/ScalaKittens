package scalakittens.experiments.word2vec

import java.util

import scala.util.Random
import math._

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
      *
      * @param other other vector
      * @param coeff coefficietn
      * @return this + coeff * other
      */
    def nudge(other: Vec, coeff: Double): Vec = {
      require(dim == other.dim)
      for (i <- data.indices) data(i) += coeff * other.data(i)
      this
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[Vec]
    
    override def equals(other: Any): Boolean = other match {
      case that: Vec =>
          dim == that.dim &&
          util.Arrays.equals(data, that.data)
      case _ => false
    }
    
    override def hashCode(): Int = {
      2017 + dim * 17 + data.hashCode()
    }

    override def toString = s"Vec(${util.Arrays.toString(data)})"
  }
  
  trait VecFactory {
    protected def dim: Int
    protected def fill(v: Array[Double]) 
    
    def apply(): Vec = {
      val v = new Array[Double](dim)
      fill(v)
      v
    }
  }
  
  object VecFactory {
    def Zero(size: Int) = new VecFactory {
      override protected def dim: Int = size

      override protected def fill(v: Array[Double]): Unit = {
        for {i <- 0 until dim} v(i) = 0.0
      }
    }
    
    def RandomCube(size: Int, seed: Long) = new VecFactory {
      override def dim: Int = size
      private val rnd = new Random(seed)

      override protected def fill(v: Array[Double]): Unit = {
        for {i <- 0 until dim} v(i) = rnd.nextDouble() * 2 - 1
      }
    }

    def RandomSphere(size: Int, seed: Long) = new VecFactory {
      override def dim: Int = size
      private val rnd = new Random(seed)

      override protected def fill(v: Array[Double]): Unit = {
        for {i <- 0 until dim} v(i) = rnd.nextDouble() * 2 - 1
        val s2 = sqrt(v map (x => x*x) sum)
        for {i <- 0 until dim} v(i) = v(i) / s2
      }
    }
  }

}
