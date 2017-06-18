package scalakittens.la

import org.specs2.mutable.Specification

import Norm._

/**
  * Created by vpatryshev on 5/7/17.
  */
class MatrixTest extends Specification {
  import Matrix._
  import math._
  
  val R0 = VectorSpace(0)
  val R2 = VectorSpace(2)
  val R3 = VectorSpace(3)
  val R4 = VectorSpace(4)
  val R5 = VectorSpace(5)
  val R7 = VectorSpace(7)
  val R10 = VectorSpace(10)
  val R25 = VectorSpace(25)
  
  def nxm(n: Int, m: Int, f: Int => Int => Double) = {
    (0 until n) map { i =>
      (0 until m) map { j =>
        f(i)(j)
      } toArray
    } toArray
  }

  case class TestMatrix[Dom <: VectorSpace, Codom <: VectorSpace](domain: Dom, codomain: Codom, f: Int => Int => Double) extends MutableMatrix[Dom, Codom] {
    override def nRows: Int = codomain.dim
    override def nCols: Int = domain.dim
    val d = nxm(nRows, nCols, f)
    override def update(i: Int, j: Int, value: Double): Unit = {
      val row = d(i)
      row(j) = value
    }

    def notImplemented: Nothing = throw new UnsupportedOperationException
    
    override def transpose: Matrix[Codom, Dom] = notImplemented
    override def +(other: Matrix[Dom, Codom]): Matrix[Dom, Codom] = notImplemented
    override def -(other: Matrix[Dom, Codom]): Matrix[Dom, Codom] = notImplemented
    override def copy: MutableMatrix[Dom, Codom] = notImplemented
    override def apply(i: Int, j: Int): Double = {
      checkIndexes(i, j)
      val row = d(i)
      row(j)
    }
  }
  
  "Matrix" should {
    "return row" in {
      TestMatrix(R10, R5, i => j => 1.0 + i * j * 1.0).row(2) ===
        R5.Vector(1.0, 3.0, 5.0, 7.0, 9.0)
      TestMatrix(R10, R0, i => j => 1.0 + i * j * 1.0).row(4) ===
        R0.Vector()
    }

    "return col" in {
      TestMatrix(R5, R25, i => j => 0.5 + i * j * 1.0).column(3) ===
        R5.Vector(0.5, 3.5, 6.5, 9.5, 12.5)
      TestMatrix(R0, R10, i => j => 1.0 + i * j * 1.0).column(4) ===
        R0.Vector()
    }

    "have foreach()" in {
      val sut = TestMatrix(R3, R4, i => j => 0.5 + i * j * 1.0)
      var log = ""
      sut.foreach((i:Int) => (j:Int) => log = log + i + j)
      log === "000102031011121320212223"
    }

    "multiply" in {
      val sut1 = TestMatrix(R3, R4, i => j => i * 10 + j)
      val sut2 = TestMatrix(R4, R5, i => j => (i - j + 1000) % 2 * 1.0)

      val expected = Matrix(R5, R3, 
        Array(4.0, 2.0, 4.0, 2.0, 4.0,
             24.0, 22.0, 24.0, 22.0, 24.0,
             44.0, 42.0, 44.0, 42.0, 44.0)
      )

      val data = new Array[Double](3 * 5)
      for {
        i <- 0 until 3
        j <- 0 until 5
      } data(i * 5 + j) = (0 until 4) map (k => sut1(i, k) * sut2(k, j)) sum

      data === Array(4.0, 2.0, 4.0, 2.0, 4.0, 24.0, 22.0, 24.0, 22.0, 24.0, 44.0, 42.0, 44.0, 42.0, 44.0)

      val product = Matrix(R3, R5, data)

      product === expected

      (sut1 * sut2) aka s"$sut1\n*\n$sut2" must_== expected
    }

    "multiply by a vector" in {
      val sut = TestMatrix(R3, R4, i => j => i * 10 + j)
      (sut * R4.Vector(0, 1, 2, 3)) === R3.Vector(14, 74, 134)
    }

    val alpha = Pi / 4
    val beta = Pi / 3

    val sampleUnitaryMatrix_3x3: R3.UnitaryMatrix = {
      R3.Unitary(
        Array(R3.Vector(cos(alpha) * cos(beta), cos(alpha) * sin(beta), sin(alpha)),
          R3.Vector(-sin(alpha) * cos(beta), -sin(alpha) * sin(beta), cos(alpha)),
          R3.Vector(sin(beta), -cos(beta), 0)
        ))
    }

    val sampleUnitaryMatrix_2x2: R2.UnitaryMatrix = R2.Unitary(
      Array(R2.Vector(cos(beta), sin(beta)),
        R2.Vector(-sin(beta), cos(beta))
      ))

    "check unitariness" in {
      val u0 = R2.Unitary(Array(R2.Vector(0, 1), R2.Vector(1, 0)))
      u0.isUnitary(0) aka s"delta = ${l2(u0 * u0.transpose - R2.UnitMatrix)}" must beTrue

      sampleUnitaryMatrix_2x2.isUnitary(0.001) aka s"delta = ${l2(sampleUnitaryMatrix_2x2 * sampleUnitaryMatrix_2x2.transpose - R2.UnitMatrix)}" must beTrue

      sampleUnitaryMatrix_3x3.isUnitary(0.001) aka s"delta = ${l2(sampleUnitaryMatrix_3x3 * sampleUnitaryMatrix_3x3.transpose - R3.UnitMatrix)}" must beTrue

    }

    "rotate" in {

      val m0 = R3.diagonalMatrix(10, 5, -1)

      val m1 = m0 rotate sampleUnitaryMatrix_3x3

      val m2 = m1 rotate sampleUnitaryMatrix_3x3.transpose

      l2(m2 - m0) < 0.00001 aka m2.toString must beTrue

      m1 === Matrix(R3, R3,
        Array(1.1250000000000009, 3.6806079660838646, 1.2500000000000002,
              3.6806079660838655, 5.375000000000001, 2.165063509461097,
              1.2500000000000002, 2.1650635094610973, 7.5))
    }
    
    "project to hyperplane" in {
      val m = R3.squareMatrix(
        Array(
          1.1250000000000009, 3.6806079660838646, 1.2500000000000002,
          3.6806079660838655, 5.375000000000001, 2.165063509461097,
          1.2500000000000002, 2.1650635094610973, 7.5))
      val sm = R3
      val sut = m.projectToHyperplane(sampleUnitaryMatrix_3x3)
      l2(sut - R3.hyperplane.diagonalMatrix(5, -1)) < 0.0001 aka sut.toString must beTrue
    }
  }

  "Matrix with hidden structure" should {
    def build(n: VectorSpace, m: VectorSpace, f: Int => Int => Double): MutableMatrix[n.type, m.type] = {
      val sut = Matrix(n, m)
      sut.nRows === n.dim
      sut.nCols === m.dim
      val content: TestMatrix[n.type, m.type] = TestMatrix(n, m, f)
      content.nRows === n.dim
      content.nCols === m.dim
      sut := content
      sut === content

      sut
    }

    "have correct number of rows" in {
      Matrix(R2,R3).nRows === 2
      val sut = build(R7, R10, i => j => 1.0+i+j)
      sut.nRows === 7
      build(R0, R2, i => j => 1.0).nRows === 0
      build(R7, R0, i => j => 1.0).nRows === 7
    }

    "have correct number of columns" in {
      Matrix(R2,R3).nCols === 3
      val sut = build(R7, R10, i => j => 1.0+i+j)
      sut.nCols === 10
      build(R0, R2, i => j => 1.0).nCols === 2
      build(R7, R0, i => j => 1.0).nCols === 0
    }

    "have apply()" in {
      val sut = build(R7, R10, i => j => 1.0+i+2*j)
      sut(0, 0) === 1.0
      sut(2, 3) === 9.0
    }
    
    "contain good data" in {
      val sut = build(R2, R3, i => j => i*10.0+j)
      sut(0,0) === 0.0
      sut(0,1) === 1.0
      sut(0,2) === 2.0
      sut(1,0) === 10.0
      sut(1,1) === 11.0
      sut(1,2) === 12.0
    }

    "return row" in {
      val sut = build(R10, R5, i => j => 1.0+i*j*1.0+j)
      sut.nRows === 10
      sut.nCols === 5
      sut.row(2) === Vector(1.0, 4.0, 7.0, 10.0, 13.0)
      build(R10, R0, i => j => 1.0+i*j*1.0).row(4) === Vector(0)
    }

    "return column" in {
      val sut: Matrix[R10.type, R5.type] = build(R10, R5, i => j => 1.0 + i * j * 1.0)
      sut.nRows === 10
      sut.nCols === 5
      sut.column(2) ===
        Vector(1.0, 3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0, 17.0, 19.0)
      build(R0, R10, i => j => 1.0+i*j*1.0).column(4) ===
        Vector(0)
    }

    "transpose" in {
      val sut0: Matrix[R10.type, R5.type] = build(R10, R5, i => j => i*10.0+j)
      sut0(1, 2) === 12.0
      sut0(2, 1) === 21.0
      sut0.column(0) === Vector(0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0)
      sut0.column(1) === Vector(1.0, 11.0, 21.0, 31.0, 41.0, 51.0, 61.0, 71.0, 81.0, 91.0)

      val sut = sut0.transpose
      sut.nCols  === 10
      sut.nRows === 5
      sut.row(0) === Vector(0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0)
      sut(1, 2) === 21.0
      
      sut.row(1) === Vector(1.0, 11.0, 21.0, 31.0, 41.0, 51.0, 61.0, 71.0, 81.0, 91.0)
      sut.row(2) === Vector(2.0, 12.0, 22.0, 32.0, 42.0, 52.0, 62.0, 72.0, 82.0, 92.0)
      sut.row(4) === Vector(4.0, 14.0, 24.0, 34.0, 44.0, 54.0, 64.0, 74.0, 84.0, 94.0)
      val sutEmpty = build(R10, R0, i => j => 1.0+i*j*1.0).transpose
      sutEmpty.nCols  === 10
      sutEmpty.nRows === 0
    }

    "copy" in {
      val sut = build(R10, R5, i => j => 1.0+i*j*1.0)
      sut(3,4) === 13.0
      val copy = sut.copy
      copy(3,4) === 13.0
      sut(3,4) = 42.5
      sut(3,4) === 42.5
      copy(3,4) === 13.0
      sut.row(3)(4) === 42.5
      sut(3,4) === 42.5
    }

    "+" in {
      val f1: Int => Int => Double = i => j => 3.0 + 2.0 * j + 11.0*i
      val mx1: Matrix[R10.type, R5.type] = build(R10, R5, f1)
      mx1.nRows === 10
      mx1(0, 1) aka mx1.toString must_== 5.0
      f1(0)(5) === 13.0

      val mx2: Matrix[R10.type, R5.type] = build(R10, R5, i => j => 1.0 - j - i)
      mx2.nRows === 10
      val sut = mx1 + mx2
      sut.nRows === 10
      sut.nCols === 5
      mx1(0, 1) aka mx1.toString must_== 5.0
      mx2(0, 1) === 0.0
      sut.foreach((i:Int) => (j:Int) => {sut(i,j) aka s"@($i,$j)" must_== 4.0 + j + 10*i; ()})
      ok
    }

    "-" in {
      val mx1: Matrix[R10.type, R5.type] = build(R10, R5, i => j => 1.0 + 2*i + 10*j)
      val mx2: Matrix[R10.type, R5.type] = build(R10, R5, i => j =>       i + 3*j)
      mx1(1, 2) === 23.0
      mx2(1, 2) === 7.0
      val sut = mx1 - mx2
      sut(1, 2) === 16.0
      sut.foreach((i:Int) => (j:Int) => {(sut(i,j) aka s"@($i,$j)") === 1.0 + i + 7*j; ()})
      ok
    }
  }
  
  "Matrix object" should {
    "build diagonal" in {
      R5.diagonalMatrix(_*5) === TestMatrix(R5, R5, i => j => if (i == j) i*5.0 else 0.0)
      R3.diagonalMatrix(-1, -2, -3) === TestMatrix(R3, R3, i => j => if (i == j) -i-1 else 0.0)
    }
    
    "build a matrix from partial function" in {
      val sut = new Matrix.OnPartialFunction(3, 10, {case (i:Int, j:Int) if j % (i+1) == 0 => i+j})
      sut === Matrix(R10, R3, Array(
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 
        1, 0, 3, 0, 5, 0, 7, 0, 9, 0, 
        2, 0, 0, 5, 0, 0, 8, 0, 0, 11))
    }
  }
}
