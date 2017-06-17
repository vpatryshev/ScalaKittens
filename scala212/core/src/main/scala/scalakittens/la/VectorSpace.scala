package scalakittens.la

import language.{implicitConversions, postfixOps, existentials}
import java.util

import scala.math.abs
import scala.util.{Random, Try}
import scalakittens.la.Matrix.DiagonalMatrix
import scalakittens.la.Norm.l2
import scalaz.Alpha.V

/**
  * Making vectors path-dependent.
  * @see [[https://stackoverflow.com/questions/44588231/path-dependent-types-without-a-path]]
  * Created by vpatryshev on 6/8/17.
  */
case class VectorSpace(dim: Int) { space =>
  lazy val hyperplane: VectorSpace = {
    require(dim > 0, "0-dimensional space does not have a hyperplane")
    VectorSpace(dim-1)
  }

  def projectToHyperplane(v: Vector): hyperplane.Vector = {
    new hyperplane.OnFunction(i => v(i+1)).asInstanceOf[hyperplane.Vector]
  }

  def injectFromHyperplane(v: hyperplane.Vector): Vector = {
    new OnFunction(i => if (i == 0) 0.0 else v(i-1))
  }
  
  require(dim > 0, s"Space dimension $dim makes no sense")

  /**
    * real-valued vector with usual operations
    *
    * @see [[http://www.javadecompilers.com]] to figure out speed issues
    *
    * Created by vpatryshev on 5/14/17.
    */

  trait Vector extends Seq[Double] with PartialFunction[Int, Double] {
//    def space = VectorSpace.this
    def length = dim
    
    lazy val range = 0 until dim

    override def isDefinedAt(i: Int) = range contains i

    def isValid: Boolean = forall(x => !x.isNaN && !x.isInfinite)

    /**
      * Infimum of two vectors
      * @param other vector
      * @return a vector whose components are minimum of the two
      */
    def inf(other: space.Vector) = {
      new OnFunction(i => math.min(apply(i), other(i)))
    }

    /**
      * Supremum of two vectors
      * @param other vector
      * @return a vector whose components are maximum of the two
      */
    def sup(other: space.Vector) = {
      new OnFunction(i => math.max(apply(i), other(i)))
    }

    /**
      * scalar product of this vector with another
      *
      * @param other another vector of the same length
      * @return the product value
      */
    def *(other: space.Vector): Double = {
      (0.0 /: range) ((s, i) => s + apply(i) * other(i))
    }

    /**
      * sum of this vector with another
      *
      * @param other another vector
      * @return a new vector, the sum of the two
      */
    def +(other: space.Vector): Vector = {
      new OnFunction(i => apply(i) + other(i))
    }

    /**
      * difference between this vector and another
      *
      * @param other another vector
      * @return a new vector, this minus other
      */
    def -(other: space.Vector): Vector = {
      new OnFunction(i => apply(i) - other(i))
    }

// this method should not exist. Instead, we need to have a function that embeds a hyperplane
//    /**
//      * Appends a value to this vector, giving a new one
//      *
//      * @param d the value
//      * @return a new vector of bigger size
//      */
//    def ::(d: Double): Vector = new OnFunction(length + 1,
//      i => if (i == 0) d else this (i - 1)
//    )

    /**
      * this vector multiplied by a scalar
      *
      * @param scalar the value by which to multiply 
      * @return a virtual vector
      */
    def *(scalar: Double): Vector = new OnFunction(i => apply(i) * scalar)

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

      range.foreach((i: Int) => v(i) = apply(i))
      v
    }

    override def iterator = range.iterator map this

    /**
      * converts a vector into a vector of norm=1 (if possible)
      *
      * @return v / norm(v)
      */
    def normalize(norm: Norm): Vector = {
      val n = norm(this)
      if (n > Double.MinPositiveValue) this / n else this
    }
    
    override def equals(other: Any): Boolean = {
      other match {
        case v: space.Vector => range.forall(i => this (i) == v(i))
        case _ => false
      }
    }

    override def toString = s"Vec(${mkString(",")})"
  }

  /**
    * Mutable version of vector
    */
  trait MutableVector extends Vector {

    /**
      * sets the i-th component value of this vector
      * used, according to scala magic, as vec(i) = newValue
      *
      * @param i the dimension
      * @param v the value
      */
    def update(i: Int, v: Double)

    /**
      * Updates this vector, making all components not bigger than those of the other vector
      * @param other vector
      */
    def decreaseBy(other: Vector): Unit = {
      for (i <- range) this(i) = math.min(this(i), other(i))
    }

    /**
      * Updates this vector, making all components not smaller than those of the other vector
      * @param other vector
      */
    def increaseBy(other: Vector) = {
      for (i <- range) this(i) = math.max(this(i), other(i))
    }

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
      for (i <- range) this(i) += other(i)
    }

    /**
      * another vector is subtracted this vector, in place
      *
      * @param other another vector
      * @return this vector, its value is now the difference of this and another
      */
    def -=(other: Vector): Unit = {
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
      this += other * coeff
    }
  }

  class OnArray(private[la] val data: Array[Double]) extends MutableVector {
    require(data.length == dim, s"Dimension of this space is $dim, array length is ${data.length}")

    /**
      * i-th component value of this vector
      *
      * @param i the index
      * @return the value
      */
    def apply(i: Int) = data(i)

    def update(i: Int, v: Double) = data(i) = v

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
    override def +=(other: space.Vector): Unit = {
      for (i <- range) data(i) += other(i)
    }

    /**
      * another vector is subtracted this vector, in place
      *
      * @param other another vector
      * @return this vector, its value is now the difference of this and another
      */
    override def -=(other: space.Vector): Unit = {
      for (i <- range) data(i) -= other(i)
    }

    /**
      * Nudge this vector in the direction of other vector, with a coefficient.
      * Modifies in place.
      *
      * @param other other vector
      * @param coeff coefficient
      * @return this + coeff * other
      */
    override def nudge(other: space.Vector, coeff: Double): Unit = {
      other match {
        case o: space.OnArray =>
          for (i <- range) data(i) += o.data(i) * coeff
        case _ =>
          for (i <- range) data(i) += other(i) * coeff
      }
    }

    /**
      * specialized version of +=
      * @param other another OnArray vector
      */
    def +=(other: space.OnArray): Unit = {
      for (i <- range) data(i) += other.data(i)
    }

    def -=(other: space.OnArray): Unit = {
      for (i <- range) data(i) -= other.data(i)
    }

    def nudge(other: space.OnArray, coeff: Double): Unit = {
      for (i <- range) data(i) += other.data(i) * coeff
    }

    override def iterator: Iterator[Double] = range.iterator map (data(_))

    override def hashCode(): Int = {
      2017 + length * 17 + data.hashCode()
    }
  }

  class OnFunction(val f: Int => Double) extends Vector {
    override def apply(i: Int) = f(i)
  }

  /**
    * constructs a vector from an array
    *
    * @param data the data
    * @return a new Vec
    */
  implicit def Vector(data: Array[Double]): MutableVector = new OnArray(data)

  /**
    * constructs a new vector in this space
    *
    * @return a new Vec
    */
  def Vector(): MutableVector = Vector(new Array[Double](dim))

  def Vector(values: Double*): MutableVector = Vector(Array(values: _*))

  /**
    * Vector factory, instantiates vectors
    */
  abstract class Factory {

    /**
      * Fills the vector with some data
      *
      * @param v the vector to fill
      */
    private[VectorSpace] def fill(v: space.MutableVector): Unit

    /**
      * instantiates a new vector
      *
      * @return a new vector
      */
    def apply(): space.Vector = {
      val v: MutableVector = Vector()
      fill(v)
      v
    }
  }

  /**
    * Creates const vector
    *
    * @return vector with the same value at each component
    */
  def const(value: Double) = new OnFunction(_ => value)

  /**
    * zero vector
    */
  val Zero = const(0.0)

  /**
    * this factory creates uniform random vectors in the cube [-1..1]<sup>size</sup>
    *
    * @param size vector length
    * @param seed random seed
    * @return random cube factory
    */
  case class RandomCube(size: Int, seed: Long) extends Factory {
    private val rnd = new Random(seed)

    override private[VectorSpace] def fill(v: MutableVector): Unit = {
      for {i <- 0 until dim} v(i) = rnd.nextDouble() * 2 - 1
    }
  }

  /**
    * this factory creates uniform random vectors on the sphere of radius 1
    *
    * @param seed random seed
    * @return random sphere factory
    */
  case class RandomSphere(seed: Long) extends Factory {
    private val cube = RandomCube(dim, seed)

    override private[VectorSpace] def fill(v: MutableVector): Unit = {

      val s2 = Stream.continually {
        cube.fill(v)
        Norm.l2(v)
      } .find {1.0 <}

      for {i <- 0 until dim
           norm <- s2
      } v(i) = v(i) / norm
    }
  }

  def unit(at: Int): Vector = {
    require(at < dim && at >= 0)
    new OnFunction(i => if (i == at) 1.0 else 0.0)
  }

  /**
    * Infimum of sequence of vectors
    * @param vectors the vectors
    * @return the vector whose components are minimum of all given vectors
    */
  def inf(vectors: TraversableOnce[Vector]): Vector = {
    val acc: MutableVector = const(Double.MaxValue).copy
    vectors foreach acc.decreaseBy
    acc
  }

  /**
    * Supremum of sequence of vectors
    * @param vectors the vectors
    * @return the vector whose components are maximum of all given vectors
    */
  def sup(vectors: TraversableOnce[Vector]): Vector = {
    val acc: MutableVector = const(Double.MinValue).copy
    vectors foreach acc.increaseBy
    acc
  }

  private val Regex = "Vec\\(\\[([\\d\\.,\\-E ]+)\\]\\)".r

  def readVector(s: String): Option[Vector] = 
    s match {
    case Regex(xs) =>
      Try {
        Vector(xs split ", ?" map (_.toDouble))
      } toOption
    case garbage => None
  }
  
  type LocalMatrix = Matrix[space.type, space.type]
  
  trait SquareMatrix extends LocalMatrix {
    override val domain = space
    override val codomain = space

    def rotate(u: UnitaryMatrix): LocalMatrix = u * this * u.transpose

    def projectToHyperplane(basis: UnitaryMatrix): hyperplane.SquareMatrix = {
      val rotatedMatrix:LocalMatrix = rotate(basis.transpose)
      new Matrix.OnFunction[hyperplane.type, hyperplane.type](hyperplane, hyperplane, (i, j) => rotatedMatrix(i+1, j+1)) with hyperplane.SquareMatrix
    }

//    def injectFromHyperplane(basis: UnitaryMatrix): hyperplane.SquareMatrix = {
//      val injected = Matrix.OnFunction[hyperplane.Vector, hyperplane.Vector](hyperplane, hyperplane, (i, j) => rotatedMatrix(i+1, j+1)) with hyperplane.SquareMatrix
//      val rotatedMatrix:Matrix[Vector, Vector] = rotate(basis.transpose)
//      rotatedMatrix.dropColumn(0).dropRow(0)
//    }

    override def *(v: Vector): MutableVector = {
      require(nCols == v.length, s"To apply a matrix to a vector we need that number of columns ($nCols) is equal to the vector's length (${v.length})")

      v match {
        case va: OnArray => byArray(va).asInstanceOf[MutableVector]
        case _ =>
          val data = rowRange map {
            i => (0.0 /: v.indices)((s, j) => s + this(i, j)*v(j))
          } toArray

          Vector(data)
      }
    }
  }

  trait UnitaryMatrix extends SquareMatrix {
    def isUnitary(precision: Double) = l2(this * transpose - UnitMatrix) <= precision

    override def *(v: Vector): MutableVector = new OnFunction(i => row(i)*v).copy

    override def transpose: UnitaryMatrix =
      new Matrix.OnFunction[space.type, space.type](space, space, (i, j) => this(j, i)) with UnitaryMatrix
  }

  /**
    * Unit matrix in this space
    */
  val UnitMatrix: UnitaryMatrix = new DiagonalMatrix[space.type](space, _ => 1.0) with UnitaryMatrix

  /**
    * An affine transform that would map a given sequence of vectors into a unit cube
    * @param vectors the vectors to transform
    * @return an AffineTransform that would map all these vectors to a unit cube
    */
  def unitCube(vectors: Iterable[Vector]): (Vector => Vector) = {
    val lowerLeft = inf(vectors)
    val upperRight = sup(vectors)
    val array = (for (i <- 0 until dim) yield {
      val d = upperRight(i) - lowerLeft(i)
      if (d > Double.MinPositiveValue) 1 / d else 0
    }).toArray
    val v = Vector(array)

    val diagonal:Matrix[space.type, space.type] = Matrix.diagonal(space, v)
    new AffineTransform[space.type, space.type](space, space)(diagonal, Zero).asInstanceOf[Vector => Vector]
  }

  /**
    * Transforms all given vectors into unit cube
    * @param vectors the vectors
    * @return an iterable of vectors inside unit cube
    */
  def toUnitCube(vectors: Iterable[Vector]): Iterable[Vector] = {
    val trans: Vector => Vector = unitCube(vectors)
    vectors map (trans(_))
  }

  /**
    * Orthonormal basis in a linear space.
    * Requires a center point and a rotation matrix
    * to transform from an original basis to this one.
    *
    * Created by vpatryshev on 5/25/17.
    */
  class Basis(val center: Vector, val rotation: UnitaryMatrix) {
    val transform = new AffineTransform[space.type, space.type](space, space)(rotation, center)
    
    def apply(v: Vector) = transform(v).asInstanceOf[Vector]
    
    /**
      * converts a vector from this basis to the original one
      *
      * @param v vector in this basis
      * @return the same vector in the old basis
      */
    def unapply(v: Vector): Vector = rotation.transpose * v + center
  }

  object Basis {
    def apply(center: Vector, basisVectors: Array[Vector]) = {
      require(basisVectors.length == dim, s"Expected $dim basis vectors, got ${basisVectors.length}")
      val columns: Array[MutableVector] = basisVectors map (_.copy)
      val matrix: UnitaryMatrix = new ColumnMatrix[space.type](space, columns) with UnitaryMatrix
      new Basis(center, matrix)
    }

    def build[V <: Vector](basisVectors: Array[V]): Basis = {
      require(basisVectors.length == dim, s"Expected $dim basis vectors, got ${basisVectors.length}")
      val columns: Array[MutableVector] = basisVectors map (_.copy)
      val matrix: UnitaryMatrix = new ColumnMatrix[space.type](space, columns) with UnitaryMatrix
      new Basis(Zero, matrix)
    }
  }

  def buildOrthonormalBasis(v: Vector): Array[Vector] = {
    val (maxValue, whereMax) = v.zipWithIndex map {case (x, i) => (abs(x), i)} max

    val vs = new Array[Vector](space.dim)

    vs(0) = v.normalize(Norm.l2).copy

    for {
      i <- 1 until v.length
    } {
      val v1: MutableVector = unit(if (i < whereMax) i-1 else i).copy
      for (j <- 0 until i) {
        v1 -= project(vs(j), v1)
      }
      vs(i) = v1.normalize(Norm.l2).copy
    }

    vs
  }

  /**
    * Project vector b to vector a
    * @param a vector to project to
    * @param b vector to project
    * @return a projection of b to a
    */
  def project(a: space.Vector, b: space.Vector) = a * ((a * b) / Norm.l2(a))

  private[la] class ColumnMatrix[Domain <: VectorSpace](val domain: Domain, val cols: Array[MutableVector]) extends Matrix[Domain, space.type] {
    override val nRows = dim
    require (domain.dim == cols.length)
    override val nCols = cols.length
    val codomain = space

    def apply(i: Int, j: Int): Double = {
      checkIndexes(i, j)
      column(j)(i)
    }

    override def column(j: Int): MutableVector = cols(j)

    override def transpose: Matrix[space.type, Domain] = new RowMatrix[Domain](domain, cols)
  }

  private[la] class RowMatrix[Codomain <: VectorSpace](val codomain: Codomain, val rows: Array[MutableVector]) extends Matrix[space.type, Codomain] {
    require (codomain.dim == rows.length)
    override val nRows = rows.length
    override val nCols = dim
    val domain = space

    def apply(i: Int, j: Int): Double = {
      checkIndexes(i, j)
      rows(i)(j)
    }

    override def row(j: Int): Vector = rows(j)

    override def transpose: Matrix[Codomain, space.type] = new ColumnMatrix[Codomain](codomain, rows)
  }
}
