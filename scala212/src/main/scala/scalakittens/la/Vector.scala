package scalakittens.la

import language.implicitConversions
import language.postfixOps
import java.util

import scala.math._
import scala.util.Random
import Norm._

/**
  * real-valued vector with usual operations
  * 
  * Created by vpatryshev on 5/14/17.
  */
class Vector(private[la] val data: Array[Double]) extends Seq[Double] {

  /**
    * length of this vector
    */
  val length: Int = data.length

  /**
    * i-th component value of this vector
    *
    * @param i the index
    * @return the value
    */
  def apply(i: Int) = data(i)

  /**
    * sets the i-th component value of this vector
    * used, according to scala magic, as vec(i) = newValue
    *
    * @param i the dimension
    * @param v the value
    */
  def update(i: Int, v: Double) = data(i) = v
  
  def map[U](f: Double => U) = data map f
  
  def copy = Vector(util.Arrays.copyOf(data, length))

  /**
    * Appends a value to this vector, giving a new one
    * @param d the value
    * @return a new vector of bigger size
    */
  def ::(d: Double) = new Vector({
    val newData = new Array[Double](length+1)
    newData(0) = d
    System.arraycopy(data, 0, newData, 1, length)
    newData
  })

  /**
    * scalar product of this vector with another
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

  /**
    * converts this vector into a vector of l2=1 (if possible)
    *
    * @return this / this.l2
    */
  def normalize: Vector = {
    val norm = Norm.l2(this)
    if (norm > Double.MinPositiveValue) this / norm else this.copy
  }
  
  def project(other: Vector) = this * ((this * other) / l2(this))
  
  def buildOrthonormalBasis: Array[Vector] = {
    val (maxValue, whereMax) = data.zipWithIndex map {case (x, i) => (abs(x), i)} max
    
    val vs = new Array[Vector](length)
    
    vs(0) = this.copy.normalize
    
    for {
      i <- 1 until length
    } {
      val v = Vector.unit(length, if (i < whereMax) i-1 else i)
      for (j <- 0 until i) v -= vs(j).project(v)
      vs(i) = v.normalize
    }
    
    vs
  }

  /**
    * Sum of all elements of this vector
    *
    * @return the sum
    */
  def sum = data sum
  
//  def canEqual(other: Any): Boolean = other.isInstanceOf[Vector]

  override def equals(other: Any): Boolean = other match {
    case that: Vector => util.Arrays.equals(data, that.data)
    case _ => false
  }

  override def hashCode(): Int = {
    2017 + length * 17 + data.hashCode()
  }

  override def toString = s"Vec(${util.Arrays.toString(data)})"

  override def iterator: Iterator[Double] = data.toList.iterator
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
  abstract class Factory(protected val dim: Int) {

    /**
      * Fills the vector with some data
 *
      * @param v the vector to fill
      */
    private[Vector] def fill(v: Vector)

    /**
      * instantiates a new vector
 *
      * @return a new vector
      */
    def apply(): Vector = {
      val v = Vector(new Array[Double](dim))
      fill(v)
      v
    }
  }

  /**
    * this factory creates zero vectors
 *
    * @param size vector length
    * @return zero factory
    */
  def Zero(size: Int) = new Factory(size) {

    override private[Vector] def fill(v: Vector): Unit = {
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
  def RandomCube(size: Int, seed: Long) = new Factory(size) {
    private val rnd = new Random(seed)

    override private[Vector] def fill(v: Vector): Unit = {
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
  def RandomSphere(size: Int, seed: Long) = new Factory(size) {
    private val cube = RandomCube(dim, seed)

    override private[Vector] def fill(v: Vector): Unit = {
      
      val s2 = Stream.continually {
        cube.fill(v)
        Norm.l2(v)
      } .find {1.0 <} head
      
      for {i <- 0 until dim} v(i) = v(i) / s2
    }
  }
  
  def FromFunction(size: Int, f: Int => Double) = new Factory(size) {
    override private[Vector] def fill(v: Vector): Unit = {
      for {i <- 0 until dim} v(i) = f(i)
    }
  }

  /**
    * Calculates 0th and 1st moments of a sequence of vectors
    *
    * @param vectors those to use in calculation
    * @return (number, sum)
    */
  def moments(vectors: Iterator[Vector]): (Int, Vector) = {
    ((1, vectors.next()) /: vectors){(moments:(Int, Vector), v) => (moments._1+1, moments._2 + v)}
  }


  /**
    * Calculates 0th and 1st moments of a sequence of vectors
    *
    * @param vectors those to use in calculation
    * @return (number, sum)
    */
  def moments(vectors: Iterable[Vector]): (Int, Vector) = moments(vectors.iterator)

  /**
    * Calculates average of a sequence of vectors
    *
    * @param vectors those to use in calculation
    * @return average
    */
  def average(vectors: Iterator[Vector]): Vector = {
    val (n, sum) = moments(vectors)
    sum / n
  }

  /**
    * Calculates average of a sequence of vectors
    *
    * @param vectors those to use in calculation
    * @return average
    */
  def average(vectors: Iterable[Vector]): Vector = average(vectors.iterator)
  
  def unit(size: Int, at: Int): Vector = {
    require(size > 0)
    require (at < size && at >= 0)
    FromFunction(size, i => if (i == at) 1.0 else 0.0)()
  }
}