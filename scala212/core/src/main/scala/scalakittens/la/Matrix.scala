package scalakittens.la

import java.util

import language.postfixOps

/**
  * Created by vpatryshev on 5/15/17.
  */
trait Matrix[Domain <: VectorSpace, Codomain <: VectorSpace] extends ((Int, Int) => Double) with Iterable[Double] {
  
  /**
    * Vector space that is domain of this matrix
    */
  val domain: Domain

  /**
    * Vector space that is Codomain of this matrix
    */
  val codomain: Codomain

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
    * i-th row of this matrix
    *
    * @param i row number
    * @return the row
    */
  def row(i: Int): Domain#Vector = {
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
  def column(j: Int): codomain.Vector = {
    val col = new Array[Double](nRows)
    rowRange foreach (i => col(i) = this(i, j))
    codomain.Vector(col)
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
  def transpose: Matrix[Codomain, Domain] = new Matrix.OnFunction[Codomain, Domain](codomain, domain, (i, j) => this(j, i))

  /**
    * copy of this matrix - this involves materialization
    *
    * @return the new matrix, mutable
    */
 def copy: MutableMatrix[Domain, Codomain] = {
    val m = Matrix[Domain, Codomain](domain, codomain)
    foreach((i:Int) => (j:Int) => m(i, j) = this(i, j))
    m
  }

  /**
    * A sum of two matrices
    *
    * @param other another matrix
    * @return the sum (virtual matrix, no space taken)
    */
  def +(other: Matrix[Domain, Codomain]): Matrix[Domain, Codomain] = {
    new Matrix.OnFunction(domain, codomain, (i, j) => this(i, j) + other(i, j))
  }

  /**
    * A difference of two matrices
    *
    * @param other another matrix
    * @return the diference (virtual matrix, no space taken)
    */
  def -(other: Matrix[Domain, Codomain]): Matrix[Domain, Codomain] = {
    new Matrix.OnFunction(domain, codomain, (i, j) => this(i, j) - other(i, j))
  }

  /**
    * Product of two matrices
    *
    * @param that another matrix
    * @return this matrix multiplied by another one; matrix is materialized
    */
  def *[NewDomain <: VectorSpace](that: Matrix[NewDomain, Domain]): Matrix[NewDomain, Codomain] = {
    val data = new Array[Double](nRows * that.nCols)
    for {
      i <- this.rowRange
      j <- that.columnRange
    } data(i*that.nCols + j) = columnRange map (k => this(i, k) * that(k, j)) sum

    val product = Matrix[NewDomain, Codomain](that.domain, codomain, data)
    
    product
  }

  /**
    * Multiplies this matrix by a vector
    *
    * @param v the vector
    * @return another vector, this * v; it so happens that it is mutable
    */
  def *(v: Domain#Vector): codomain.Vector = {
    require(nCols == v.length, s"To apply a matrix to a vector we need that number of columns ($nCols) is equal to the vector's length (${v.length})")
    
    v match {
      case va: domain.OnArray => byArray(va).asInstanceOf[codomain.Vector] // todo: fixit
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
  protected def byArray(v: VectorSpace#OnArray): codomain.MutableVector = {
    val data = rowRange map {
      i => (0.0 /: (0 until v.length))((s, j) => s + this(i, j)*v.data(j))
    } toArray
    
    codomain.Vector(data)
  }
  
  override def equals(x: Any): Boolean = {
    x match {
      case other: Matrix[Domain, Codomain] =>
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

trait MutableMatrix[Domain <: VectorSpace, Codomain <: VectorSpace] extends Matrix[Domain, Codomain] {

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
  def :=(other: Matrix[Domain, Codomain]): this.type = {
    foreach(i => j => this(i, j) = other(i, j))
  }
}

object Matrix {
  private[la] def apply[Domain <: VectorSpace, Codomain <: VectorSpace](domain: Domain, codomain: Codomain, storage: Array[Double]): MutableMatrix[Domain, Codomain] = 
    new OnArray[Domain, Codomain](domain, codomain, storage)

  /**
    * Builds a mutable matrix
    *
    * @param domain the space of rows
    * @param codomain the space of columns
    * @return a new matrix (mutable)
    */
  def apply[Domain <: VectorSpace, Codomain <: VectorSpace](domain: Domain, codomain: Codomain): MutableMatrix[Domain, Codomain] = {
    apply(domain, codomain, storage = new Array[Double](domain.dim * codomain.dim))
  }
    
  class OnArray[Domain <: VectorSpace, Codomain <: VectorSpace](val domain: Domain, val codomain: Codomain, protected val data: Array[Double]) extends MutableMatrix[Domain, Codomain] {
    
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
    override def copy: MutableMatrix[Domain, Codomain] = Matrix[Domain, Codomain](domain, codomain, util.Arrays.copyOf(data, data.length))
  }

  /**
    * A matrix which values are supplied by a function. That's lightweight if the function is.
    *
    * @param domain matrix domain (space of rows)
    * @param codomain matrix Codomain (space of columns)
    * @param f the function that gives matrix values
    */
  class OnFunction[Domain <: VectorSpace, Codomain <: VectorSpace]
    (val domain: Domain, val codomain: Codomain, f: (Int, Int) => Double) extends Matrix[Domain, Codomain] {

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
    * @param codomain matrix Codomain (space of columns)
    * @param pf the partial function that gives matrix values if defined, all other values are 0.
    */
  class OnPartialFunction[Domain <: VectorSpace, Codomain <: VectorSpace](val domain: Domain, val codomain: Codomain, pf: PartialFunction[(Int, Int), Double]) extends Matrix[Domain, Codomain] {

    override def apply(i: Int, j: Int): Double = {
      checkIndexes(i, j)
      if (pf.isDefinedAt((i, j))) pf((i, j)) else 0.0
    }
  }

  /**
    * Zero matrix
 *
    * @param domain the space of rows
    * @param codomain the space of columns
    * @return a zero matrix of given dimensions
    */
  def Zero(domain: VectorSpace, codomain: VectorSpace): Matrix[domain.type, codomain.type] = 
    new OnFunction[domain.type, codomain.type](domain, codomain, (i, j) => 0.0)
}
