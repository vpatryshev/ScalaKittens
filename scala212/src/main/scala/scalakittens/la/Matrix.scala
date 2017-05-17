package scalakittens.la

import java.util
import util.Arrays._

/**
  * Created by vpatryshev on 5/15/17.
  */
trait Matrix {
  /**
    * @return number of rows
    */
  def nRows: Int

  /**
    * 
    * @return number of columns
    */
  def nCols: Int

  /**
    * the value at row i and column j
    *
    * @param i row number
    * @param j column number
    * @return the value
    */
  def apply(i: Int, j: Int): Double

  protected def requireCompatibility(other: Matrix): Matrix = {
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
    for (j <- 0 until nCols) row(j) = apply(i, j)
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
    for (i <- 0 until nRows) col(i) = this(i, j)
    Vector(col)
  }

  def foreach(op: Int => Int => Unit): Matrix = {
    for {
      i <- 0 until nRows
      j <- 0 until nCols
    } op(i)(j)
    
    this
  }
  
  /**
    * Transposed matrix
    * 
    * @return the new matrix
    */
  def transpose: Matrix

  /**
    * copy of this matrix
    *
    * @return the new matrix
    */
  def copy: Matrix

  /**
    * A sum of two matrices
    *
    * @param other another matrix
    * @return the sum
    */
  def +(other: Matrix): Matrix

  /**
    * generic value setter (we are probably mutable
    *
    * @param i row number
    * @param j column umber
    * @param value value to set
    */
  private[la] def set(i: Int, j: Int, value: Double)

  /**
    * Adds aonther matrix to this one
    *
    * @param other another matrix
    * @return this one, modified
    */
  def +=(other: Matrix): Matrix = {
    requireCompatibility(other)
    foreach(i => j => set(i, j, this(i, j) + other(i, j)))
  }
  
  def :=(other: Matrix): Matrix = {
    requireCompatibility(other)
    foreach(i => j => set(i, j, other(i, j)))
  }
  
  override def equals(x: Any): Boolean = {
    x match {
      case other: Matrix =>
        nRows == other.nRows &&
        nCols == other.nCols && {
          foreach(i => j => if (this(i, j) != other(i, j)) return false)
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

object Matrix {

  /**
    * Builds a matrix out of given rows
    *
    * @param width the width of the matrix. Need it, since rows can be empty
    * @param rows rows of the matrix
    * @return a matrix built from the rows
    */
  def ofRows(width: Int, rows: Array[Vector]): Matrix = new Matrix {
    require(width >= 0, s"Bad width $width")
    val nRows = rows.length
    val nCols = width

    require (rows.forall(_.length == nCols), s"expected a rectangular matrix of width $nCols, got something wrong; nCols=$nCols")

    def apply(i: Int, j: Int): Double = row(i)(j)

    /**
      * overrides row(), returning the actual data row (mutable!)
      *
      * @param i row number
      * @return the row
      */
    override def row(i: Int): Vector = rows(i)

    def transpose: Matrix = Matrix.ofColumns(nCols, rows)

    def copy: Matrix = {
      val newRows = rows map (_.copy)
      Matrix.ofRows(nCols, newRows)
    }
    
    def +(other: Matrix): Matrix = {
      requireCompatibility(other)
      val newRows = new Array[Vector](nRows)

      for (i <- 0 until nRows) newRows(i) = row(i) + other.row(i)

      Matrix.ofRows(nCols, newRows)
    }

    private[la] def set(i: Int, j: Int, value: Double) = rows(i).data(j) = value

    override def +=(other: Matrix): Matrix = {
      requireCompatibility(other)
      foreach(i => j => rows(i).data(j) += other(i, j))
    }
  }

  /**
    * Builds a matrix out of given columns
    *
    * @param height the height of the matrix. Need it, since colss can be empty
    * @param cols colunnss of the matrix
    * @return a matrix built from the columns
    */
  def ofColumns(height: Int, cols: Array[Vector]): Matrix = new Matrix {
    require(height >= 0, s"Bad height $height")
    val nRows = height
    val nCols = cols.length

    require (cols.forall(_.length == nRows), s"expected a rectangular matrix of height $nRows, got something wrong")

    def apply(i: Int, j: Int): Double = column(j)(i)

    override def column(j: Int): Vector = cols(j)

    def transpose: Matrix = Matrix.ofRows(nRows, cols)

    def copy: Matrix = {
      val newCols = cols map (_.copy)
      Matrix.ofRows(nRows, newCols)
    }

    def +(other: Matrix): Matrix = {
      requireCompatibility(other)
      val newCols = new Array[Vector](nCols)

      for (j <- 0 until nCols) newCols(j) = column(j) + other.column(j)

      Matrix.ofColumns(nRows, newCols)
    }

    private[la] def set(i: Int, j: Int, value: Double) = cols(j).data(i) = value

    override def +=(other: Matrix): Matrix = {
      requireCompatibility(other)
      foreach(i => j => cols(j).data(i) += other(i, j))
    }
  }

  /**
    * Builds a matrix of given width and height
 *
    * @param height the height
    * @param width the width 
    * @return a new matrix (mutable)
    */
  def apply(height: Int, width: Int): Matrix = {
    require(width >= 0, s"Bad width $width")
    require(height >= 0, s"Bad height $height")
    Matrix(height, width, Vector(width * height))
  }
    
  private def apply(height: Int, width: Int, dataSource: Vector): Matrix = new Matrix {

    private val data = dataSource

    private def index(i: Int, j: Int) = i*width+j

    override def nRows: Int = height

    override def nCols: Int = width
    
    private[la] def set(i: Int, j: Int, value: Double): Unit = 
      data.data(index(i,j)) = value

    override def transpose: Matrix = {
      val mx = Matrix(width, height)
      foreach(i => j => mx.set(j, i, this(i, j)))
      mx
    }

    override def +(other: Matrix): Matrix = {
      requireCompatibility(other)
      val mx = Matrix(height, width)
      foreach(i => j => mx.set(i, j, this(i, j) + other(i, j)))
      mx
    }

    /**
      * copy of this matrix
      *
      * @return the new matrix
      */
    override def copy: Matrix = {
      requireCompatibility(Matrix(height, width, data.copy))
    }

    override def apply(i: Int, j: Int): Double = data(index(i, j))
  }
  
  def Zero(height: Int, width: Int): Matrix = apply(height, width, Vector.Zero(height*width)())
  
  def covariance(in: Iterable[Vector]): Matrix = {
    val avg = Vector.average(in)
    val m = Zero(avg.length, avg.length)
    for (v <- in) {
      for {i <- 0 until m.nRows
           j <- 0 until m.nCols
      } m.set(i, j, m(i,j) + (v(i)-avg(i))*(v(j)-avg(j)))
    }
    m
  }
}
