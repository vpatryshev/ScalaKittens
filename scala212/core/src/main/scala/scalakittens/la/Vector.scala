package scalakittens.la

import language.implicitConversions
import language.postfixOps
import java.util

import scala.util.Random

/**
  * real-valued vector with usual operations
  *
  * Created by vpatryshev on 5/14/17.
  */

trait Vector extends Seq[Double] with PartialFunction[Int, Double] {

  lazy val range = 0 until length

  override def isDefinedAt(i: Int) = range contains i

  def isValid: Boolean = forall (x => !x.isNaN && !x.isInfinite)

  /**
    * Checks the compatibility of another vector with this one
    *
    * @param other vector
    * @tparam T vector type 
    * @return that other vector
    */
  protected def requireCompatibility[T <: Vector](other: T): Unit = {
    require(length == other.length, 
      s"need the length, have $length, and another has ${other.length}")
  }

  /**
    * scalar product of this vector with another
    *
    * @param other another vector of the same length
    * @return the product value
    */
  def *(other: Vector): Double = {
    requireCompatibility(other)
    (0.0 /: range)((s, i) => s + this(i) * other(i))
  }
    /**
      * sum of this vector with another
      *
      * @param other another vector
      * @return a new vector, the sum of the two
      */
    def +(other: Vector): Vector = {
      requireCompatibility(other)
      new Vector.OnFunction(length, i => this(i) + other(i))
    }

    /**
      * difference between this vector and another
      *
      * @param other another vector
      * @return a new vector, this minus other
      */
    def -(other: Vector): Vector = {
      requireCompatibility(other)
      new Vector.OnFunction(length, i => this(i) - other(i))
    }

    /**
      * Appends a value to this vector, giving a new one
      *
      * @param d the value
      * @return a new vector of bigger size
      */
    def ::(d: Double): Vector = new Vector.OnFunction(length + 1,
      i => if (i == 0) d else this(i-1)
    )
    /**
      * this vector multiplied by a scalar
      *
      * @param scalar the value by which to multiply 
      * @return a virtual vector
      */
    def *(scalar: Double): Vector = new Vector.OnFunction(length, i => this(i)*scalar)

    /**
      * this vector divided by a scalar
      *
      * @param scalar the value by which to divide 
      * @return a virtual vector
      */
    def /(scalar: Double): Vector = *(1.0 / scalar)

    /**
      * Materialized copy of this vector
      *
      * @return a new vector with its data stored somewhere
      */
    def copy: MutableVector = {
      val v = Vector(length)

      range.foreach((i:Int) => v(i) = this(i))
      v
    }

  override def equals(other: Any): Boolean = {
    other match {
      case v: Vector => length == v.length && range.forall(i => this(i) == v(i))
      case _ => false
    }
  }
  
  override def toString = s"Vec(${mkString(",")})"
}

/**
  * Mutable version of vector
  */
trait MutableVectorT extends Vector {

  /**
    * sets the i-th component value of this vector
    * used, according to scala magic, as vec(i) = newValue
    *
    * @param i the dimension
    * @param v the value
    */
  def update(i: Int, v: Double)

  /**
    * this vector is multiplied by a scalar, in place
    *
    * @param scalar the value by which to multiply 
    * @return this, now all its values are multiplied by scalar
    */
  def *=(scalar: Double): Unit = {
    for (i <- range) this(i) *= scalar
  }

  /**
    * this vector divided by a scalar, in place
    *
    * @param scalar the value by which to divide 
    * @return this, now all its values are divided by scalar
    */
  def /=(scalar: Double): Unit = {
    for (i <- range) this(i) /= scalar
  }

  /**
    * another vector is added to this vector, in place
    *
    * @param other another vector
    * @return this vector, its value is now the sum of this and another
    */
  def +=(other: Vector): Unit = {
    requireCompatibility(other)
    for (i <- range) this(i) += other(i)
  }

  /**
    * another vector is subtracted this vector, in place
    *
    * @param other another vector
    * @return this vector, its value is now the difference of this and another
    */
  def -=(other: Vector): Unit = {
    requireCompatibility(other)
    for (i <- range) this(i) -= other(i)
  }

  /**
    * Nudge this vector in the direction of other vector, with a coefficient.
    * Modifies in place.
    *
    * @param other other vector
    * @param coeff coefficient
    * @return this + coeff * other
    */
  def nudge(other: Vector, coeff: Double): Unit = {
    requireCompatibility(other)
    this += other * coeff
  }
}

class MutableVector(private val data: Array[Double]) extends MutableVectorT {
  
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

  def update(i: Int, v: Double) = data(i) = v

  def map[U](f: Double => U) = data map f

  override def copy: MutableVector = Vector(util.Arrays.copyOf(data, length))

  /**
    * this vector is multiplied by a scalar, in place
    *
    * @param scalar the value by which to multiply 
    * @return this, now all its values are multiplied by scalar
    */
  override def *=(scalar: Double): Unit = {
    for (i <- range) data(i) *= scalar
  }

  /**
    * this vector divided by a scalar, in place
    *
    * @param scalar the value by which to divide 
    * @return this, now all its values are divided by scalar
    */
  override def /=(scalar: Double): Unit = {
    for (i <- range) data(i) /= scalar
  }

  /**
    * another vector is added to this vector, in place
    *
    * @param other another vector
    * @return this vector, its value is now the sum of this and another
    */
  override def +=(other: Vector): Unit = {
    requireCompatibility(other)
    for (i <- range) data(i) += other(i)
  }

  /**
    * another vector is subtracted this vector, in place
    *
    * @param other another vector
    * @return this vector, its value is now the difference of this and another
    */
  override def -=(other: Vector): Unit = {
    requireCompatibility(other)
    for (i <- range) data(i) -= other(i)
  }

  override def hashCode(): Int = {
    2017 + length * 17 + data.hashCode()
  }

  /**
    * Nudge this vector in the direction of other vector, with a coefficient.
    * Modifies in place.
    *
    * @param other other vector
    * @param coeff coefficient
    * @return this + coeff * other
    */
  override def nudge(other: Vector, coeff: Double): Unit = {
    requireCompatibility(other)
    for (i <- range) this(i) += other(i)*coeff
  }
  
  override def toString = s"Vec(${util.Arrays.toString(data)})"

  override def iterator: Iterator[Double] = data.iterator
}

object Vector {
  /**
    * constructs a vector from an array
    *
    * @param data the data
    * @return a new Vec
    */
  implicit def apply(data: Array[Double]): MutableVector = new MutableVector(data)

  /**
    * constructs a new vector of a given size
    *
    * @param size the size
    * @return a new Vec
    */
  def apply(size: Int): MutableVector = apply(new Array[Double](size))

  def apply(values: Double*): MutableVector = apply(Array(values:_*))

  /**
    * Vector factory, instantiates vectors
    */
  abstract class Factory(protected val dim: Int) {

    /**
      * Fills the vector with some data
      *
      * @param v the vector to fill
      */
    private[Vector] def fill(v: MutableVector): Unit

    /**
      * instantiates a new vector
      *
      * @return a new vector
      */
    def apply(): Vector = {
      val v: MutableVector = Vector(new Array[Double](dim))
      fill(v)
      v
    }
  }

  /**
    * Creates zero vectors
    *
    * @param size vector length
    * @return zero vector
    */
  def Zero(size: Int) = new OnFunction(size, _ => 0.0)

  /**
    * this factory creates uniform random vectors in the cube [-1..1]<sup>size</sup>
    *
    * @param size vector length
    * @param seed random seed
    * @return random cube factory
    */
  case class RandomCube(size: Int, seed: Long) extends Factory(size) {
    private val rnd = new Random(seed)

    override private[Vector] def fill(v: MutableVector): Unit = {
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
  case class RandomSphere(size: Int, seed: Long) extends Factory(size) {
    private val cube = RandomCube(dim, seed)

    override private[Vector] def fill(v: MutableVector): Unit = {

      val s2 = Stream.continually {
        cube.fill(v)
        Norm.l2(v)
      } .find {1.0 <} head

      for {i <- 0 until dim} v(i) = v(i) / s2
    }
  }

  def unit(size: Int, at: Int): Vector = {
    require ((size == 0 || at < size) && at >= 0)
    new OnFunction(size, i => if (i == at) 1.0 else 0.0)
  }

  private val Regex = "Vec\\(\\[([\\d\\.,\\-E ]+)\\]\\)".r

  def read(s: String) = s match {
    case Regex(xs) =>
      val numbers = xs split ", ?" map (_.toDouble)
      Some(Vector(numbers))
    case garbage => None
  }

  class OnFunction(val length: Int, val f: Int => Double) extends Vector {
    override def apply(i: Int) = f(i)

    override def iterator = (0 until length iterator) map f
  }
}