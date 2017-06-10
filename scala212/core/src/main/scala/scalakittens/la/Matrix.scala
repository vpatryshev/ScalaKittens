package scalakittens.la

import java.util

import language.postfixOps
import scalakittens.la.Matrix._
import scalakittens.la.Norm._
import VectorSpace._

/**
  * Created by vpatryshev on 5/15/17.
  */
trait Matrix extends ((Int, Int) => Double) with Iterable[Double] {
  type DomainVector
  type CoomainVector
  
  /**
    * Vector space that is domain of this matrix
    */
  val domain: VectorSpace

  /**
    * Vector space that is codomain of this matrix
    */
  val codomain: VectorSpace

  /**
    * @return number of rows
    */
  def nRows: Int = codomain.dim
  
  lazy val rowRange = 0 until nRows
  
  /**
    * 
    * @return number of columns
    */
  def nCols: Int = domain.dim

  lazy val columnRange = 0 until nCols

  /**
    * Checks row and column indexes
 *
    * @param i row index
    * @param j column index
    */
  def checkIndexes(i: Int, j: Int) = 
    require(rowRange.contains(i) && columnRange.contains(j), s"Bad indexes ($i, $j), matrix $nRows⨯$nCols")

  /**
    * the value at row i and column j
    *
    * @param i row number
    * @param j column number
    * @return the value
    */
  def apply(i: Int, j: Int): Double

  /**
    * Checks the compatibility of another matrix with this one
 *
    * @param other matrix
    * @tparam T matrix type 
    * @return that other matrix
    */
  protected def requireCompatibility[T <: Matrix](other: T): T = {
    require(nRows == other.nRows, s"need the same number of rows, have $nRows, and another has ${other.nRows}")
    require(nCols == other.nCols, s"need the same number of columns, have $nCols, and another has ${other.nCols}")
    other
  }
  
  /**
    * i-th row of this matrix
    *
    * @param i row number
    * @return the row
    */
  def row(i: Int): domain.MutableVector = {
    val row = new Array[Double](nCols)
    columnRange foreach (j => row(j) = this(i, j))
    domain.Vector(row)
  }

  /**
    * j-th column of this matrix
    *
    * @param j column number
    * @return the column
    */
  def column(j: Int): codomain.MutableVector = {
    val col = new Array[Double](nRows)
    rowRange foreach (i => col(i) = this(i, j))
    codomain.Vector(col)
  }

  /**
    * Produces a new matrix where this row is deleted
    *
    * @param rowNo the number of the row to delete
    * @return a new matrix (virtual)
    */
  def dropRow(rowNo: Int): Matrix = {
    require(rowRange contains rowNo)

    new Matrix.OnFunction(domain, codomain.hyperplane, (i, j) => if (i < rowNo) this(i, j) else this(i+1, j))
  }

  /**
    * Produces a new matrix where this column is deleted
    *
    * @param columnNo the number of the column to delete
    * @return a new matrix (virtual)
    */
  def dropColumn(columnNo: Int): Matrix = {
    require(rowRange contains columnNo)

    new Matrix.OnFunction(domain, codomain.hyperplane, (i, j) => if (j < columnNo) this(i, j) else this(i, j + 1))
  }

  def allElements: Seq[Double] = for {
    i <- rowRange
    j <- columnRange
  } yield this(i, j)

  def iterator = {
    for {
      i <- rowRange
      j <- columnRange
    } yield this(i, j)
  } iterator
  
  /**
    * applies an operation to each pair of row index and column index
    *
    * @param op the operation (result is ignored)
    * @return this matrix
    */
  def foreach(op: Int => Int => Unit): this.type = {
    for {
      i <- rowRange
      j <- columnRange
    } op(i)(j)
    this
  }
  
  /**
    * Transposed matrix
    * 
    * @return the new matrix; it is virtual, you need to call copy to materialize it
    */
  def transpose: Matrix = new Matrix.OnFunction(codomain, domain, (i, j) => this(j, i))

  /**
    * copy of this matrix - this involves materialization
    *
    * @return the new matrix, mutable
    */
 def copy: MutableMatrix = {
    val m = Matrix(domain, codomain)
    foreach((i:Int) => (j:Int) => m(i, j) = this(i, j))
    m
  }

  /**
    * A sum of two matrices
    *
    * @param other another matrix
    * @return the sum (virtual matrix, no space taken)
    */
  def +(other: Matrix): Matrix = {
    requireCompatibility(other)
    new Matrix.OnFunction(domain, codomain, (i, j) => this(i, j) + other(i, j))
  }

  /**
    * A difference of two matrices
    *
    * @param other another matrix
    * @return the diference (virtual matrix, no space taken)
    */
  def -(other: Matrix): Matrix = {
    requireCompatibility(other)
    new Matrix.OnFunction(domain, codomain, (i, j) => this(i, j) - other(i, j))
  }

  /**
    * Product of two matrices
    *
    * @param that another matrix
    * @return this matrix multiplied by another one; matrix is materialized
    */
  def *(that: Matrix): Matrix = {
    require(nCols == that.nRows, s"For a product we need that number of columns ($nCols) is equal to the other matrix' number of rows (${that.nRows}")
    val data = new Array[Double](nRows * that.nCols)
    for {
      i <- this.rowRange
      j <- that.columnRange
    } data(i*that.nCols + j) = columnRange map (k => this(i, k) * that(k, j)) sum

    val product = Matrix(that.domain, codomain, data)
    
    product
  }

  /**
    * Multiplies this matrix by a vector
    *
    * @param v the vector
    * @return another vector, this * v; it so happens that it is mutable
    */
  def *(v: domain.Vector): codomain.MutableVector = {
    require(nCols == v.length, s"To apply a matrix to a vector we need that number of columns ($nCols) is equal to the vector's length (${v.length})")
    
    v match {
      case va: domain.OnArray => byArray(va)
      case _ =>
        val data = rowRange map {
          i => (0.0 /: v.indices)((s, j) => s + this(i, j)*v(j))
        } toArray
        
        codomain.Vector(data)
    }
  }

  /**
    * Specialization of vector multiplication
    * @param v vector on array
    * @return product of this matrix and the array
    */
  protected def byArray(v: domain.OnArray): codomain.MutableVector = {
    require(nCols == v.length, s"To apply a matrix to a vector we need that number of columns ($nCols) is equal to the vector's length (${v.length})")

    val data = rowRange map {
      i => (0.0 /: (0 until v.length))((s, j) => s + this(i, j)*v.data(j))
    } toArray
    
    codomain.Vector(data)
  }
  
  def rotate(u: codomain.UnitaryMatrix): Matrix = u * this * u.transpose

  def projectToHyperplane(basis: codomain.UnitaryMatrix): Matrix = {
    val rotatedMatrix = rotate(basis.transpose)
    rotatedMatrix.dropColumn(0).dropRow(0)
  }
  
  override def equals(x: Any): Boolean = {
    x match {
      case other: Matrix =>
        nRows == other.nRows &&
        nCols == other.nCols && {
          foreach((i:Int) => (j:Int) => if (this(i, j) != other(i, j)) return false)
          true
        }
      case _ => false
    }
  }
  
  override def toString = {
    val out = new StringBuilder
    out append "["
    for (i <- rowRange) {
      out append "["
      out.append(row(i) mkString ",")
      out append "]\n"
    }
    out append "]"
    out.toString
  }
}

trait MutableMatrix extends Matrix {

  /**
    * generic value setter
    *
    * @param i row number
    * @param j column umber
    * @param value value to set
    */
  def update(i: Int, j: Int, value: Double)

  /**
    * copies values of another matrix into this one
    *
    * @param other another matrix
    * @return
    */
  def :=(other: Matrix): this.type = {
    requireCompatibility(other)
    foreach(i => j => this(i, j) = other(i, j))
  }
}

object Matrix {
  private[la] def apply(domain: VectorSpace, codomain: VectorSpace, storage: Array[Double]): MutableMatrix = 
    new OnArray(domain, codomain, storage)

  /**
    * Builds a mutable matrix of given width and height
    *
    * @param domain the space of rows
    * @param codomain the space of columns
    * @return a new matrix (mutable)
    */
  def apply(domain: VectorSpace, codomain: VectorSpace): MutableMatrix = {
    apply(domain, codomain, storage = new Array[Double](domain.dim * codomain.dim))
  }
    
  class OnArray(val domain: VectorSpace, val codomain: VectorSpace, protected val data: Array[Double]) extends MutableMatrix {

    type DomainVector = domain.Vector

    type CodomainVector = codomain.Vector
    
    private def index(i: Int, j: Int) = {
      checkIndexes(i, j)
      i*nCols+j
    }

    override def update(i: Int, j: Int, value: Double): Unit = {
      checkIndexes(i, j)
      data(index(i,j)) = value
    }

    override def apply(i: Int, j: Int): Double = {
      checkIndexes(i, j)
      data(index(i, j))
    }

    /**
      * copy of this matrix
      *
      * @return the new matrix
      */
    override def copy: MutableMatrix = Matrix(domain, codomain, util.Arrays.copyOf(data, data.length))
  }

  /**
    * A matrix which values are supplied by a function. That's lightweight if the function is.
    *
    * @param domain matrix domain (space of rows)
    * @param codomain matrix codomain (space of columns)
    * @param f the function that gives matrix values
    */
  class OnFunction(val domain: VectorSpace, val codomain: VectorSpace, f: (Int, Int) => Double) extends Matrix {

    override def apply(i: Int, j: Int): Double = {
      checkIndexes(i, j)
      f(i, j)
    }
  }

  /**
    * A matrix which values are supplied by a partial function. That's lightweight if the function is.
    * If the function is not defined on a specific combination of row and column, the value is 0.
    *
    * @param domain matrix domain (space of rows)
    * @param codomain matrix codomain (space of columns)
    * @param pf the partial function that gives matrix values if defined, all other values are 0.
    */
  class OnPartialFunction(val domain: VectorSpace, val codomain: VectorSpace, pf: PartialFunction[(Int, Int), Double]) extends Matrix {

    override def apply(i: Int, j: Int): Double = {
      checkIndexes(i, j)
      if (pf.isDefinedAt((i, j))) pf((i, j)) else 0.0
    }
  }
  
  private def diagonalize(f: Int => Double): PartialFunction[(Int, Int), Double] =
  { case (i, j) if i == j => f(i) }

  /**
    * Diagonal matrix of given size; source provides values. It's virtual.
    *
    * @param space the vector space in which the matrix acts
    * @param source whatever function that provides matrix values for the diagonal, the rest is 0
    */
  class DiagonalMatrix(space: VectorSpace, source: Int => Double) 
    extends Matrix.OnPartialFunction(space, space, diagonalize(source))

  /**
    * Zero matrix
 *
    * @param domain the space of rows
    * @param codomain the space of columns
    * @return a zero matrix of given dimensions
    */
  def Zero(domain: VectorSpace, codomain: VectorSpace): Matrix = 
    new OnFunction(domain, codomain, (i, j) => 0.0)

  /**
    * builds a diagonal matrix of given size; source provides values. It's virtual.
    *
    * @param space the vector space in which the matrix acts
    * @param source whatever function that provides matrix values for the diagonal, the rest is 0
    * @return the diagonal matrix
    */
  def diagonal(space: VectorSpace, source: Int => Double) = new DiagonalMatrix(space, source)
}
