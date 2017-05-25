package scalakittens.la

import language.postfixOps
import language.reflectiveCalls
import math._
import scalakittens.la.Matrix._
import scalakittens.la.Norm._
import scalakittens.stats.AccumulatingMoments

/**
  * Created by vpatryshev on 5/15/17.
  */
trait Matrix extends ((Int, Int) => Double) with Iterable[Double] {
  /**
    * @return number of rows
    */
  def nRows: Int
  
  lazy val rowRange = 0 until nRows
  
  /**
    * 
    * @return number of columns
    */
  def nCols: Int

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
  def row(i: Int): Vector = {
    val row = new Array[Double](nCols)
    columnRange foreach (j => row(j) = this(i, j))
    Vector(row)
  }

  /**
    * j-th column of this matrix
    *
    * @param j column number
    * @return the column
    */
  def column(j: Int): Vector = {
    val col = new Array[Double](nRows)
    rowRange foreach (i => col(i) = this(i, j))
    Vector(col)
  }

  /**
    * Produces a new matrix where this row is deleted
    *
    * @param rowNo the number of the row to delete
    * @return a new matrix (virtual)
    */
  def dropRow(rowNo: Int): Matrix = {
    require(rowRange contains rowNo)

    new Matrix.OnFunction(nRows - 1, nCols, (i, j) => if (i < rowNo) this(i, j) else this(i+1, j))
  }

  /**
    * Produces a new matrix where this column is deleted
    *
    * @param columnNo the number of the column to delete
    * @return a new matrix (virtual)
    */
  def dropColumn(columnNo: Int): Matrix = {
    require(rowRange contains columnNo)

    new Matrix.OnFunction(nRows, nCols - 1, (i, j) => if (j < columnNo) this(i, j) else this(i, j + 1))
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
  def transpose: Matrix = new Matrix.OnFunction(nCols, nRows, (i, j) => this(j, i))

  /**
    * copy of this matrix - this involves materialization
    *
    * @return the new matrix, mutable
    */
 def copy: MutableMatrix = {
    val m = Matrix(nRows, nCols)
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
    new Matrix.OnFunction(nRows, nCols, (i, j) => this(i, j) + other(i, j))
  }

  /**
    * A difference of two matrices
    *
    * @param other another matrix
    * @return the diference (virtual matrix, no space taken)
    */
  def -(other: Matrix): Matrix = {
    requireCompatibility(other)
    new Matrix.OnFunction(nRows, nCols, (i, j) => this(i, j) - other(i, j))
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
      i <- 0 until nRows
      j <- 0 until that.nCols
    } data(i*that.nCols + j) = (0 until nCols) map (k => this(i, k) * that(k, j)) sum

    val product = Matrix(nRows, that.nCols, Vector(data))
    
    product
  }

  /**
    * Multiplies this matrix by a vector
    *
    * @param v the vector
    * @return another vector, this * v
    */
  def *(v: Vector): Vector = {
    require(nCols == v.length, s"For a product we need that number of columns ($nCols) is equal to the vector's length (${v.length})")

    0 until nRows map {
      i => (0.0 /: (0 until v.length))((s, j) => s + this(i, j)*v(j))
    } toArray
  }
  
  def rotate(u: UnitaryMatrix): Matrix = u * this * u.transpose

  def projectToHyperplane(basis: UnitaryMatrix): Matrix = {
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
    for (i <- 0 until nRows) {
      out append "["
      out.append(row(i).data mkString ",")
      out append "]\n"
    }
    out append "]"
    out.toString
  }
}

trait MutableMatrix extends Matrix {


  private def normalizeColumn(j: Int): Unit = {
    val norm = l2(rowRange map (i => this(i, j)))
    
    if (norm > Double.MinPositiveValue) rowRange foreach (i => this(i, j) /= norm)
  }

  private[la] def normalizeVertically(): Unit = {
    0 until nCols foreach normalizeColumn
  }

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

trait UnitaryMatrix extends Matrix {
  def isUnitary(precision: Double) = l2(this * transpose - Unit(nCols)) <= precision
  
  override def transpose: UnitaryMatrix = 
    new Matrix.OnFunction(nCols, nRows, (i, j) => this(j, i)) with UnitaryMatrix
}

object Matrix {

  def Unitary(basis: Array[Vector]) = new ColumnMatrix(basis.length, basis) with UnitaryMatrix {
    require(basis.nonEmpty)
    require(basis.forall(_.length == basis.length))
  }

  def Unitary(basis: List[Vector]) = new ColumnMatrix(basis.length, basis.toArray) with UnitaryMatrix {
    require(basis.nonEmpty)
    require(basis.forall(_.length == basis.length))
  }

  /**
    * Builds a matrix out of given rows
    *
    * @param width the width of the matrix. Need it, since rows can be empty
    * @param rows rows of the matrix
    * @return a matrix built from the rows
    */
  def ofRows(width: Int, rows: Array[Vector]): MutableMatrix = new MutableMatrix {
    require(width >= 0, s"Bad width $width")
    val nRows = rows.length
    val nCols = width

    require (rows.forall(_.length == nCols), s"expected a rectangular matrix of width $nCols, got something wrong; nCols=$nCols")

    def apply(i: Int, j: Int): Double = {
      checkIndexes(i, j)
      row(i)(j)
    }

    /**
      * overrides row(), returning the actual data row (mutable!)
      *
      * @param i row number
      * @return the row
      */
    override def row(i: Int): Vector = rows(i)

    override def transpose: Matrix = Matrix.ofColumns(nCols, rows)

    override def copy: MutableMatrix = {
      val newRows = rows map (_.copy)
      Matrix.ofRows(nCols, newRows)
    }

    override def update(i: Int, j: Int, value: Double) = {
      checkIndexes(i, j)
      rows(i).data(j) = value
    }

    override def *(v: Vector): Vector = {
      require(nCols == v.length, s"For a product we need that number of columns ($nCols) is equal to the vector's length (${v.length})")
      rows map (_ * v)
    }
  }

  /**
    * Builds a matrix out of given columns
    *
    * @param height the height of the matrix. Need it, since colss can be empty
    * @param columns colunnss of the matrix
    * @return a matrix built from the columns
    */
  def ofColumns(height: Int, columns: Array[Vector]): MutableMatrix = 
    new ColumnMatrix(height, columns) with MutableMatrix
  
  private[la] class ColumnMatrix(val height: Int, val cols: Array[Vector]) extends MutableMatrix {
    require(height >= 0, s"Bad height $height")
    val nRows = height
    val nCols = cols.length

    require (cols.forall(_.length == nRows), s"expected a rectangular matrix of height $nRows, got something wrong")

    def apply(i: Int, j: Int): Double = {
      checkIndexes(i, j)
      column(j)(i)
    }

    override def column(j: Int): Vector = cols(j)

    override def transpose: Matrix = Matrix.ofRows(nRows, cols)

    override def copy: MutableMatrix = {
      val newCols = cols map (_.copy)
      Matrix.ofRows(nRows, newCols)
    }

    override def update(i: Int, j: Int, value: Double) = {
      checkIndexes(i, j)
      cols(j).data(i) = value
    }
  }

  /**
    * Builds a mutable matrix of given width and height
 *
    * @param height the height
    * @param width the width 
    * @return a new matrix (mutable)
    */
  def apply(height: Int, width: Int): MutableMatrix = {
    require(width >= 0, s"Bad width $width")
    require(height >= 0, s"Bad height $height")
    apply(height, width, new Array[Double](height * width))
  }

  private[la] def apply(height: Int, width: Int, dataSource: Vector): MutableMatrix = 
    new OnVector(height, width, dataSource)
    
  class OnVector(val nRows: Int, val nCols: Int, protected val data: Vector) extends MutableMatrix {

    private def index(i: Int, j: Int) = {
      checkIndexes(i, j)
      i*nCols+j
    }

    override def update(i: Int, j: Int, value: Double): Unit = {
      checkIndexes(i, j)
      data.data(index(i,j)) = value
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
    override def copy: MutableMatrix = Matrix(nRows, nCols, data.copy)
  }

  /**
    * A matrix which values are supplied by a function. That's lightweight if the function is.
    *
    * @param nRows matrix height
    * @param nCols matrix width
    * @param f the function that gives matrix values
    */
  class OnFunction(val nRows: Int, val nCols: Int, f: (Int, Int) => Double) extends Matrix {

    override def apply(i: Int, j: Int): Double = {
      checkIndexes(i, j)
      f(i, j)
    }
  }

  /**
    * A matrix which values are supplied by a partial function. That's lightweight if the function is.
    * If the function is not defined on a specific combination of row and column, the value is 0.
    *
    * @param nRows matrix height
    * @param nCols matrix width
    * @param pf the partial function that gives matrix values if defined, all other values are 0.
    */
  class OnPartialFunction(val nRows: Int, val nCols: Int, pf: PartialFunction[(Int, Int), Double]) extends Matrix {

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
    * @param size matrix size (it's square)
    * @param source whatever function that provides matrix values for the diagonal, the rest is 0
    */
  class DiagonalMatrix(size: Int, source: Int => Double) 
    extends Matrix.OnPartialFunction(size, size, diagonalize(source))

  /**
    * Zero matrix
 *
    * @param height matrix height
    * @param width matrix width
    * @return a zero matrix of given dimensions
    */
  def Zero(height: Int, width: Int): Matrix = new OnFunction(height, width, (i, j) => 0.0)

  /**
    * Unit matrix
 *
    * @param size matrix size
    * @return a unit matrix of a given size
    */
  def Unit(size: Int): UnitaryMatrix = new DiagonalMatrix(size, _ => 1.0) with UnitaryMatrix

  /**
    * builds a diagonal matrix of given size; source provides values. It's virtual.
 *
    * @param size matrix size (it's square)
    * @param source whatever function that provides matrix values for the diagonal, the rest is 0
    * @return the diagonal matrix
    */
  def diagonal(size: Int, source: Int => Double) = new DiagonalMatrix(size, source)

  /**
    * builds a diagonal matrix; source vector provides values. It's virtual.
 *
    * @param source the vector that specifies values on the diagonal; all others are 0
    * @return the diagonal matrix
    */
  def diagonal(source: Vector): Matrix = diagonal(source.length, source)

  /**
    * builds a diagonal matrix from given values.
 *
    * @param values the values on the matrix diagonal (all others are 0)
    * @return a diagonal matrix
    */
  def diagonal(values: Double*): Matrix = diagonal(Vector(values:_*))

  /**
    * Calculates covariance matrix for a given Iterable of vectors
    * The iterable is scanned twice, first for the average, and then for the matrix
    * 
    * @param in stream of vectors (must be same size)
    * @return covariance matrix
    */
  def covariance(size: Int, in: Iterable[Vector]): (Vector, Matrix) = {
    val acc = new AccumulatingMoments(size)
    val data = new Array[Double](size * size)
    for {
      v <- in
    } acc += v
    (acc.avg, acc.covariance)
  }
}
