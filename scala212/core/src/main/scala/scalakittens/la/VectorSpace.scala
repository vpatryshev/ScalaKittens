package scalakittens.la

import language.{implicitConversions, postfixOps}
import java.util

import scala.math.abs
import scala.util.{Random, Try}
import scalakittens.la.Norm.l2

/**
  * Making vectors path-dependent.
  * @see [[https://stackoverflow.com/questions/44588231/path-dependent-types-without-a-path]]
  * Created by vpatryshev on 6/8/17.
  */
case class VectorSpace(dim: Int) { space =>
  lazy val hyperplane: VectorSpace = {
    require(dim >= 0, "0-dimensional space does not have a hyperplane")
    VectorSpace(dim-1)
  }

  def projectToHyperplane(v: Vector): hyperplane.Vector = {
    new hyperplane.OnFunction(i => v(i+1)).asInstanceOf[hyperplane.Vector]
  }

  def injectFromHyperplane(v: VectorSpace#Vector): Vector = {
    // the following check runs in runtime, who knows where is Hyperplane coming from
    require(v.length + 1 == dim)
    new OnFunction(i => if (i == 0) 0.0 else v(i-1))
  }
  
  require(dim >= 0, s"Space dimension $dim makes no sense")

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
    def inf(other: Vector) = {
      new OnFunction(i => math.min(apply(i), other(i)))
    }

    /**
      * Supremum of two vectors
      * @param other vector
      * @return a vector whose components are maximum of the two
      */
    def sup(other: Vector) = {
      new OnFunction(i => math.max(apply(i), other(i)))
    }

    /**
      * scalar product of this vector with another
      *
      * @param other another vector of the same length
      * @return the product value
      */
    def *[V <: space.Vector](other: V): Double = {
      (0.0 /: range) ((s, i) => s + apply(i) * other(i))
    }

    /**
      * sum of this vector with another
      *
      * @param other another vector
      * @return a new vector, the sum of the two
      */
    def +(other: Vector): Vector = {
      new OnFunction(i => apply(i) + other(i))
    }

    /**
      * difference between this vector and another
      *
      * @param other another vector
      * @return a new vector, this minus other
      */
    def -(other: Vector): Vector = {
      new OnFunction(i => apply(i) - other(i))
    }

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

    def /(components: Int => Double): Vector = new OnFunction(i => this(i) / components(i))

    /**
      * Materialized copy of this vector
      *
      * @return a new vector with its data stored somewhere
      */
    def copy: MutableVector = {
      val data: Array[Double] = new Array[Double](dim)
      range.foreach((i: Int) => data(i) = apply(i))
      new OnArray(data)
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
        case v: VectorSpace#Vector => 
          length == v.length && 
            range.forall(i => {this(i) == v(i)
        })
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
      * scalar product of this vector with another
      *
      * @param other another vector of the same length
      * @return the product value
      */
    override def *[V <: space.Vector](other: V): Double = {
      other match {
        case o: OnArray => ArrayOps.scalarProduct(data, o.data)
        case _ =>
          (0.0 /: range) ((s, i) => s + data(i) * other(i))
      }
    }

    /**
      * this vector is multiplied by a scalar, in place
      *
      * @param scalar the value by which to multiply 
      * @return this, now all its values are multiplied by scalar
      */
    override def *=(scalar: Double): Unit = {
      ArrayOps.multBy(data, scalar)
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
      other match {
        case o: OnArray => this += o
        case _ =>
          for (i <- range) data(i) += other(i)
      }
    }

    /**
      * another vector is subtracted this vector, in place
      *
      * @param other another vector
      * @return this vector, its value is now the difference of this and another
      */
    override def -=(other: Vector): Unit = {
      other match {
        case o: OnArray => this -= o
        case _ =>
          for (i <- range) data(i) -= other(i)
      }
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
      other match {
        case o: OnArray => nudge(o, coeff)
        case _ =>
          for (i <- range) data(i) += other(i) * coeff
      }
    }

    /**
      * specialized version of +=
      * @param other another OnArray vector
      */
    def +=(other: OnArray): Unit = {
      ArrayOps.addTo(data, other.data)
    }

    def -=(other: OnArray): Unit = {
      ArrayOps.subtractFrom(data, other.data)
    }

    def nudge(other: OnArray, coeff: Double): Unit = {
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
    private[VectorSpace] def fill(v: MutableVector): Unit

    /**
      * instantiates a new vector
      *
      * @return a new vector
      */
    def apply(): Vector = {
      val v: MutableVector = new OnArray(new Array[Double](dim))
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
    * @param seed random seed
    * @return random cube factory
    */
  case class RandomCube(seed: Long) extends Factory {
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
    private val cube = RandomCube(seed)

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

  private lazy val Regex = "Vec\\(\\[?([\\d\\.,\\-E ]+)\\]?\\)".r

  def readVector(s: String): Option[Vector] = 
    s match {
    case Regex(xs) =>
      Try {
        Vector(xs split ", ?" map (_.toDouble))
      } toOption
    case garbage => None
  }
  
  type LocalMatrix = Matrix[space.type, space.type]
  
  def squareMatrix(f: (Int, Int) => Double): SquareMatrix =
    new Matrix.OnFunction[space.type, space.type](space, space, f) with SquareMatrix

//  private def squareMatrix(f: PartialFunction[(Int, Int), Double]): SquareMatrix =
//    new Matrix.OnPartialFunction[space.type, space.type](space, space, f) with SquareMatrix

  def squareMatrix(data: Array[Double]): SquareMatrix = squareMatrix((i, j) => data(i*dim+j))
  
  trait SquareMatrix extends LocalMatrix {
    override val domain = space
    override val codomain = space
    
    def triangle = new TriangularMatrix(this)
    
    def rotate(u: UnitaryMatrix): SquareMatrix = {
      val half = u * this
      val transposed = u.transpose
      val product = half * transposed
      squareMatrix(product)
    }

    def projectToHyperplane(basis: UnitaryMatrix = UnitMatrix): hyperplane.SquareMatrix = {
      val rotatedMatrix:LocalMatrix = rotate(basis.transpose)
      val newMatrix = hyperplane.squareMatrix((i, j) => rotatedMatrix(i + 1, j + 1))
      newMatrix.asInstanceOf[hyperplane.SquareMatrix]
    }

    override def *[V <: space.Vector](v: V): space.Vector = {
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

    override def *[V <: space.Vector](v: V): space.Vector = new OnFunction(i => row(i)*v).copy

    override def transpose: UnitaryMatrix =
      new Matrix.OnFunction[space.type, space.type](space, space, (i, j) => this(j, i)) with UnitaryMatrix
  }

  /**
    * Unit matrix in this space
    */
  val UnitMatrix: UnitaryMatrix = new DiagonalMatrix(_ => 1.0) with UnitaryMatrix

  /**
    * Unitary matrix built from given basis vectors
    * @param basis vectors of this basis
    * @return the matrix
    */
  def unitaryMatrix(basis: Seq[Vector]): UnitaryMatrix = 
    new ColumnMatrix[space.type](space, basis map (_.copy)) with UnitaryMatrix {
      require(basis.length == dim)
    }

  private def diagonalize(f: Int => Double): PartialFunction[(Int, Int), Double] =
  { case (i, j) if i == j => f(i) }

  /**
    * Diagonal matrix of given size; source provides values. It's virtual.
    *
    * @param source whatever function that provides matrix values for the diagonal, the rest is 0
    */
  class DiagonalMatrix(source: Int => Double)
    extends Matrix.OnPartialFunction[space.type, space.type](space, space, diagonalize(source)) with SquareMatrix

  /**
    * builds a diagonal matrix of given size; source provides values. It's virtual.
    *
    * @param source whatever function that provides matrix values for the diagonal, the rest is 0
    * @return the diagonal matrix
    */
  def diagonalMatrix(source: Int => Double): SquareMatrix = new DiagonalMatrix(source)

  def diagonalMatrix(values: Double*): SquareMatrix = diagonalMatrix(values)
  
  class TriangularMatrix(source: SquareMatrix) extends Matrix.OnArray[space.type, space.type](space, space, new Array[Double](dim*(dim+1)/2)) with SquareMatrix {

    override def checkArray() = ()

    override def index(i0: Int, j0: Int): Int = {
      checkIndexes(i0, j0)
      val (i, j) = if (i0 < j0) (j0, i0) else (i0, j0)
      val idx = i*(i+1)/2 + j
      idx
    }

    for { i <- rowRange
          j <- 0 to i
    } this(i, j) = source(i, j)

    /**
      * copy of this matrix
      *
      * @return the new matrix
      */
    override def copy: MutableMatrix[space.type, space.type] = new TriangularMatrix(this)
  }

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

    val diagonal:SquareMatrix = diagonalMatrix(v)
    new AffineTransform[space.type, space.type](space, space)(diagonal, lowerLeft)
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
  case class Basis(center: Vector, rotation: UnitaryMatrix) extends (Vector => Vector) {
    val transform = new AffineTransform[space.type, space.type](space, space)(rotation, center)
    
    def apply(v: Vector) = transform(v)
    
    /**
      * converts a vector from this basis to the original one
      *
      * @param v vector in this basis
      * @return the same vector in the old basis
      */
    def unapply(v: Vector): Vector = rotation.transpose * v + center
  }
  
  object Basis {
    
    def apply(center: Vector, basisVectors: Array[MutableVector]): Basis = {
      require(basisVectors.length == dim, s"Expected $dim basis vectors, got ${basisVectors.length}")
      val matrix: UnitaryMatrix = unitaryMatrix(basisVectors)
      new Basis(center, matrix)
    }

    def apply(center: Vector, basisVectors: Array[Vector]): Basis = {
      apply(center, basisVectors map (_.copy))
    }

    def apply[V <: Vector](basisVectors: Array[V]): Basis = {
      require(basisVectors.length == dim, s"Expected $dim basis vectors, got ${basisVectors.length}")
      val columns: Array[MutableVector] = basisVectors map (_.copy)
      val matrix: UnitaryMatrix = new ColumnMatrix[space.type](space, columns) with UnitaryMatrix
      new Basis(Zero, matrix)
    }
  }

  def buildOrthonormalBasis(v: Vector): Array[Vector] = {
    val (maxValue, whereMax) = v.zipWithIndex map {case (x, i) => (abs(x), i)} max

    val vs = new Array[Vector](dim)

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
  def project[V <: space.Vector](a: Vector, b: V) = a * ((a * b) / Norm.l2(a))

  private[la] class ColumnMatrix[Domain <: VectorSpace](val domain: Domain, val cols: Seq[MutableVector]) extends Matrix[Domain, space.type] {
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

  class RowMatrix[Codomain <: VectorSpace](val codomain: Codomain, val rows: Seq[MutableVector]) extends Matrix[space.type, Codomain] {
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

object Spaces {
  lazy val R0 = VectorSpace(0)
  lazy val R1 = VectorSpace(1)
  lazy val R2 = VectorSpace(2)
  lazy val R3 = VectorSpace(3)
  lazy val R4 = VectorSpace(4)
  lazy val R5 = VectorSpace(5)
  lazy val R6 = VectorSpace(6)
  lazy val R7 = VectorSpace(7)
  lazy val R8 = VectorSpace(8)
  lazy val R9 = VectorSpace(9)
  lazy val R10 = VectorSpace(10)
  lazy val R11 = VectorSpace(11)
  lazy val R12 = VectorSpace(12)
  lazy val R13 = VectorSpace(13)
  lazy val R14 = VectorSpace(14)
  lazy val R15 = VectorSpace(15)
  lazy val R16 = VectorSpace(16)
  lazy val R17 = VectorSpace(17)
  lazy val R18 = VectorSpace(18)
  lazy val R19 = VectorSpace(19)
  lazy val R20 = VectorSpace(20)
  lazy val R21 = VectorSpace(21)
  lazy val R22 = VectorSpace(22)
  lazy val R23 = VectorSpace(23)
  lazy val R24 = VectorSpace(24)
  lazy val R25 = VectorSpace(25)
  lazy val R26 = VectorSpace(26)
  lazy val R27 = VectorSpace(27)
  lazy val R28 = VectorSpace(28)
  lazy val R29 = VectorSpace(29)
  lazy val R30 = VectorSpace(30)
  lazy val R31 = VectorSpace(31)
  lazy val R32 = VectorSpace(32)
  lazy val R33 = VectorSpace(33)
  lazy val R34 = VectorSpace(34)
  lazy val R35 = VectorSpace(35)
  lazy val R36 = VectorSpace(36)
  lazy val R100 = VectorSpace(100)
}
