package scalakittens.la

import language.implicitConversions
import java.util

import scala.math._
import scala.util.Random
import scalaz.Alpha.P

/**
  * real-valued vector with usual operations
  * 
  * Created by vpatryshev on 5/14/17.
  */
class Vector(private[la] val data: Array[Double]) {
  
  /**
    * length of this vector
    */
  val length: Int = data.length

  /**
    * i-th component value of this vector
    *
    * @param i the dimension
    * @return the value
    */
  def apply(i: Int) = data(i)
  
  def foreach(f: Double => Unit) = data foreach f
  def forall(p: Double => Boolean) = data forall p
  def exists(p: Double => Boolean) = data exists p
  def map[U](f: Double => U) = data map f
  def /:[B](z: B)(op: (B, Double) => B): B = (z/:data)(op)
  
  def copy = Vector(util.Arrays.copyOf(data, length))

  /**
    * scalar product of this vector with the other
 *
    * @param other another vector of the same length
    * @return the product value
    */
  def *(other: Vector): Double = {
    require(length == other.length)
    (0.0 /: (0 until length))((s, i) => s + data(i) * other.data(i))
  }

  /**
    * this vector multiplied by a scalar
 *
    * @param scalar the value by which to multiply 
    * @return a new vector
    */
  def *(scalar: Double): Vector = new Vector(data map (scalar *))

  /**
    * this vector divided by a scalar
    *
    * @param scalar the value by which to divide 
    * @return a new vector
    */
  def /(scalar: Double): Vector = new Vector(data map (_/scalar))

  /**
    * sum of this vector with another
 *
    * @param other another vector
    * @return a new vector, the sum of the two
    */
  def +(other: Vector): Vector = {
    require(length == other.length)
    new Vector(data zip other.data map {case (a,b) => a+b})
  }

  /**
    * difference between this vector and another
 *
    * @param other another vector
    * @return a new vector, this minus other
    */
  def -(other: Vector): Vector = {
    require(length == other.length)
    new Vector((data zip other.data) map {case (a,b) => a-b})
  }

  /**
    * this vector is multiplied by a scalar, in place
 *
    * @param scalar the value by which to multiply 
    * @return this, now all its values are multiplied by scalar
    */
  def *=(scalar: Double): Vector = {
    for (i <- data.indices) data(i) *= scalar
    this
  }

  /**
    * this vector divided by a scalar, in place
    *
    * @param scalar the value by which to divide 
    * @return this, now all its values are divided by scalar
    */
  def /=(scalar: Double): Vector = {
    for (i <- data.indices) data(i) /= scalar
    this
  }

  /**
    * another vector is added to this vector, in place
 *
    * @param other another vector
    * @return this vector, its value is now the sum of this and another
    */
  def +=(other: Vector): Vector = {
    require(length == other.length)
    for (i <- data.indices) data(i) += other.data(i)
    this
  }

  /**
    * another vector is subtracted this vector, in place
 *
    * @param other another vector
    * @return this vector, its value is now the difference of this and another
    */
  def -=(other: Vector): Vector = {
    require(length == other.length)
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
  def nudge(other: Vector, coeff: Double): Vector = {
    require(length == other.length)
    for (i <- data.indices) data(i) += coeff * other.data(i)
    this
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Vector]

  override def equals(other: Any): Boolean = other match {
    case that: Vector => util.Arrays.equals(data, that.data)
    case _ => false
  }

  override def hashCode(): Int = {
    2017 + length * 17 + data.hashCode()
  }

  override def toString = s"Vec(${util.Arrays.toString(data)})"
}

object Vector {

  implicit def vec(data: Array[Double]): Vector = new Vector(data)
  
  /**
    * constructs a vector from an array
 *
    * @param data the data
    * @return a new Vec
    */
  def apply(data: Array[Double]): Vector = new Vector(data)

  /**
    * constructs a new vector of a given size
 *
    * @param size the size
    * @return a new Vec
    */
  def apply(size: Int): Vector = apply(new Array[Double](size))

  def apply(values: Double*): Vector = apply(Array(values:_*))
  
  /**
    * Vector factory, instantiates vectors
    */
  trait Factory {

    /**
      * length of the vectors the factory instantiates
 *
      * @return the length
      */
    protected def dim: Int

    /**
      * Fills the vector with some data
 *
      * @param v the vector to fill
      */
    private[Vector] def fill(v: Array[Double])

    /**
      * instantiates a new vector
 *
      * @return a new vector
      */
    def apply(): Vector = {
      val v = new Array[Double](dim)
      fill(v)
      Vector(v)
    }
  }

  /**
    * this factory creates zero vectors
 *
    * @param size vector length
    * @return zero factory
    */
  def Zero(size: Int) = new Factory {
    override protected def dim: Int = size

    override private[Vector] def fill(v: Array[Double]): Unit = {
      for {i <- 0 until dim} v(i) = 0.0
    }
  }

  /**
    * this factory creates uniform random vectors in the cube [-1..1]<sup>size</sup>
 *
    * @param size vector length
    * @param seed random seed
    * @return random cube factory
    */
  def RandomCube(size: Int, seed: Long) = new Factory {
    override def dim: Int = size
    private val rnd = new Random(seed)

    override private[Vector] def fill(v: Array[Double]): Unit = {
      for {i <- 0 until dim} v(i) = rnd.nextDouble() * 2 - 1
    }
  }

  /**
    * this factory creates uniform random vectors on the sphere of radius 1
    *
    * @param size vector length
    * @param seed random seed
    * @return random sphere factory
    */
  def RandomSphere(size: Int, seed: Long) = new Factory {
    override def dim: Int = size
    private val cube = RandomCube(dim, seed)

    override private[Vector] def fill(v: Array[Double]): Unit = {
      val s2 = Stream.continually {
        cube.fill(v)
        sqrt((0.0 /: v.toStream)((s,x) => s + x*x))
      } .dropWhile {1.0 <} head
      
      for {i <- 0 until dim} v(i) = v(i) / s2
      ()
    }
  }
  
  def average(vectors: Iterator[Vector]): Vector = {
    val (n, sum) = 
      ((1, vectors.next()) /: vectors){(moments:(Int, Vector), v) => (moments._1+1, moments._2 + v)}
    
    sum / n
  }
  
  def average(vectors: Array[Vector]): Vector = average(vectors.iterator)
}