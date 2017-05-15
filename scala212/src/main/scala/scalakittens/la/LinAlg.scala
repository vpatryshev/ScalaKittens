package scalakittens.la

import java.util

import scala.math._
import scala.util.Random

/**
  * Linear Algebra ops (typeclass?), Scala version
  * Created by vpatryshev on 5/7/17.
  */
object LinAlg {
  
  
  case class Matrix(nRows: Int, nCols: Int, rows: Array[Vector]) {
    require (rows.length == nRows, s"expected $nRows rows, have ${rows.length}")
    require (rows.forall(_.length == nCols), s"expected a rectangular matrix of length $nCols, got something wrong")
    
    def transpose: Matrix = {
      val newRows = new Array[Vector](nCols)
      for (i <- 0 until nCols) {
        val newRow = new Array[Double](nRows)
        newRows(i) = new Vector(newRow)
        for (j <- 0 until nRows) newRow(j) = rows(j)(i)
      }
      
      Matrix(nCols, nRows, newRows)
    }
    
    def copy: Matrix = {
      val newRows = new Array[Vector](nRows)
      for (i <- 0 until nRows) {
        newRows(i) = Vector(nCols)
        System.arraycopy(rows(i), 0, newRows(i), 0, nCols)
      }

      Matrix(nRows, nCols, newRows)
    }

    def +(other: Matrix): Matrix = {
      require(nRows == other.nRows)
      require(nCols == other.nCols)
      val newRows = new Array[Vector](nRows)
      
      for (i <- 0 until nRows) {
        val newRow = new Array[Double](nCols)
        newRows(i) = Vector(newRow)
        for (j <- 0 until nCols) newRow(j) = rows(i)(j) + other.rows(i)(j)
      }

      Matrix(nRows, nCols, newRows)
    }

    def +=(other: Matrix): Matrix = {
      require(nRows == other.nRows)
      require(nCols == other.nCols)
      for {
        i <- 0 until nRows
        j <- 0 until nCols
      } rows(i).data(j) += other.rows(i)(j)
      
      this
    }
  }


}
