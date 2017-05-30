package scalakittens.la

import org.specs2.mutable.Specification
import Norm._

/**
  * Created by vpatryshev on 5/7/17.
  */
class MatrixTest extends Specification {
  import Matrix._
  import math._
  
  def nxm(n: Int, m: Int, f: Int => Int => Double) = {
    (0 until n) map { i =>
      (0 until m) map { j =>
        f(i)(j)
      } toArray
    } toArray
  }

  case class TestMatrix(h: Int, w: Int, f: Int => Int => Double) extends MutableMatrix {
    override def nRows: Int = h
    override def nCols: Int = w
    val d = nxm(h, w, f)
    override def update(i: Int, j: Int, value: Double): Unit = {
      val row = d(i)
      row(j) = value
    }

    def notImplemented: Nothing = throw new UnsupportedOperationException
    
    override def transpose: Matrix = notImplemented
    override def +(other: Matrix): Matrix = notImplemented
    override def -(other: Matrix): Matrix = notImplemented
    override def copy: MutableMatrix = notImplemented
    override def apply(i: Int, j: Int): Double = {
      checkIndexes(i, j)
      val row = d(i)
      row(j)
    }
  }
  
  "Matrix" should {
    "return row" in {
      TestMatrix(10, 5, i => j => 1.0 + i * j * 1.0).row(2) must_==
        Vector(1.0, 3.0, 5.0, 7.0, 9.0)
      TestMatrix(10, 0, i => j => 1.0 + i * j * 1.0).row(4) must_==
        Vector(0)
    }

    "return col" in {
      TestMatrix(5, 25, i => j => 0.5 + i * j * 1.0).column(3) must_==
        Vector(0.5, 3.5, 6.5, 9.5, 12.5)
      TestMatrix(0, 10, i => j => 1.0 + i * j * 1.0).column(4) must_==
        Vector(0)
    }

    "have foreach()" in {
      val sut = TestMatrix(3, 4, i => j => 0.5 + i * j * 1.0)
      var log = ""
      sut.foreach((i:Int) => (j:Int) => log = log + i + j)
      log must_== "000102031011121320212223"
    }

    "normalize vertically" in {
      val sut = TestMatrix(3, 4, i => j => 0.5 + i * 10.0 + j)

      sut.normalizeVertically()

      sut must_== Matrix.ofRows(4, Array(
        Vector(0.021703261485649127, 0.06140377126253595, 0.09667364890456637, 0.12807973746712817),
        Vector(0.45576849119863166, 0.4707622463461089, 0.48336824452283184, 0.49402184451606584),
        Vector(0.8898337209116142, 0.8801207214296819, 0.8700628401410972, 0.8599639515650035)
      ))

      ok
    }

    "multiply" in {
      val sut1 = TestMatrix(3, 4, i => j => i * 10 + j)
      val sut2 = TestMatrix(4, 5, i => j => (i - j + 1000) % 2 * 1.0)

      val expected: MutableMatrix = Matrix.ofRows(5, Array(
        Vector(4.0, 2.0, 4.0, 2.0, 4.0),
        Vector(24.0, 22.0, 24.0, 22.0, 24.0),
        Vector(44.0, 42.0, 44.0, 42.0, 44.0)
      ))

      val data = new Array[Double](3 * 5)
      for {
        i <- 0 until 3
        j <- 0 until 5
      } data(i * 5 + j) = (0 until 4) map (k => sut1(i, k) * sut2(k, j)) sum

      data must_== Array(4.0, 2.0, 4.0, 2.0, 4.0, 24.0, 22.0, 24.0, 22.0, 24.0, 44.0, 42.0, 44.0, 42.0, 44.0)

      val product = Matrix(3, 5, Vector(data))

      product must_== expected

      sut1 * sut2 aka s"$sut1\n*\n$sut2" must_== expected
    }

    "multiply by a vector" in {
      val sut = TestMatrix(3, 4, i => j => i * 10 + j)
      sut * Vector(0, 1, 2, 3) must_== Vector(14, 74, 134)
    }

    val alpha = Pi / 4
    val beta = Pi / 3

    val sampleUnitaryMatrix_3x3: UnitaryMatrix = {
      Matrix.Unitary(
        Array(Vector(cos(alpha) * cos(beta), cos(alpha) * sin(beta), sin(alpha)),
          Vector(-sin(alpha) * cos(beta), -sin(alpha) * sin(beta), cos(alpha)),
          Vector(sin(beta), -cos(beta), 0)
        ))
    }

    val sampleUnitaryMatrix_2x2: UnitaryMatrix = Matrix.Unitary(
      Array(Vector(cos(beta), sin(beta)),
        Vector(-sin(beta), cos(beta))
      ))

    "check unitariness" in {
      val u0 = Matrix.Unitary(Array(Vector(0, 1), Vector(1, 0)))
      u0.isUnitary(0) aka s"delta = ${l2(u0 * u0.transpose - Unit(2))}" must beTrue

      sampleUnitaryMatrix_2x2.isUnitary(0.001) aka s"delta = ${l2(sampleUnitaryMatrix_2x2 * sampleUnitaryMatrix_2x2.transpose - Unit(3))}" must beTrue

      sampleUnitaryMatrix_3x3.isUnitary(0.001) aka s"delta = ${l2(sampleUnitaryMatrix_3x3 * sampleUnitaryMatrix_3x3.transpose - Unit(3))}" must beTrue

    }

    "rotate" in {

      val m0 = diagonal(10, 5, -1)

      val m1 = m0 rotate sampleUnitaryMatrix_3x3

      val m2 = m1 rotate sampleUnitaryMatrix_3x3.transpose

      l2(m2 - m0) < 0.00001 aka m2.toString must beTrue

      m1 must_== Matrix.ofRows(3,
        Array(Vector(1.1250000000000009, 3.6806079660838646, 1.2500000000000002),
             Vector(3.6806079660838655, 5.375000000000001, 2.165063509461097),
             Vector(1.2500000000000002, 2.1650635094610973, 7.5)))
    }

    "drop a row" in {
      val sut = Matrix.ofRows(3, Array(
        Vector(1, 0.5, 1.0 / 3),
        Vector(-0.5, 1, -0.5),
        Vector(-1.0 / 3, 0.5, 1.0)
      ))

      sut.dropRow(0) must_== Matrix.ofRows(3, Array(
        Vector(-0.5, 1, -0.5),
        Vector(-1.0 / 3, 0.5, 1.0)
      ))

      sut.dropRow(1) must_== Matrix.ofRows(3, Array(
        Vector(1, 0.5, 1.0 / 3),
        Vector(-1.0 / 3, 0.5, 1.0)
      ))

      sut.dropRow(2) must_== Matrix.ofRows(3, Array(
        Vector(1, 0.5, 1.0 / 3),
        Vector(-0.5, 1, -0.5)
      ))
    }

    "drop a column" in {
      val sut = Matrix.ofColumns(3, Array(
        Vector(1, 0.5, 1.0 / 3),
        Vector(-0.5, 1, -0.5),
        Vector(-1.0 / 3, 0.5, 1.0)
      ))

      sut.dropColumn(0) must_== Matrix.ofColumns(3, Array(
        Vector(-0.5, 1, -0.5),
        Vector(-1.0 / 3, 0.5, 1.0)
      ))

      sut.dropColumn(1) must_== Matrix.ofColumns(3, Array(
        Vector(1, 0.5, 1.0 / 3),
        Vector(-1.0 / 3, 0.5, 1.0)
      ))

      sut.dropColumn(2) must_== Matrix.ofColumns(3, Array(
        Vector(1, 0.5, 1.0 / 3),
        Vector(-0.5, 1, -0.5)
      ))
    }
    
    "project to hyperplane" in {
      val m = Matrix.ofRows(3,
        Array(
          Vector(1.1250000000000009, 3.6806079660838646, 1.2500000000000002),
          Vector(3.6806079660838655, 5.375000000000001, 2.165063509461097),
          Vector(1.2500000000000002, 2.1650635094610973, 7.5)))
      val sut = m.projectToHyperplane(sampleUnitaryMatrix_3x3)
      l2(sut - diagonal(5, -1)) < 0.0001 aka sut.toString must beTrue
    }
  }
  
  "Matrix of rows" should {
    def build(n: Int, m: Int, f: Int => Int => Double): MutableMatrix = 
      Matrix.ofRows(m, nxm(n, m, f) map Vector.apply)
    
    "have correct number of rows" in {
      val sut = build(7, 10, i => j => 1.0+i+j)
      sut.nRows must_== 7
      Matrix.ofRows(10, Array()).nRows must_== 0
      build(7, 0, i => j => 1.0).nRows must_== 7
    }
    
    "have apply()" in {
      val sut = build(7, 10, i => j => 1.0+i+j)
      sut(0, 0) must_== 1.0
      sut(2, 3) must_== 6.0
    }
    
    "return row" in {
      build(10, 5, i => j => 1.0+i*j*1.0).row(2) must_==
        Vector(1.0, 3.0, 5.0, 7.0, 9.0)
      build(10, 0, i => j => 1.0+i*j*1.0).row(4) must_==
        Vector(0)
    }

    "return column" in {
      build(10, 5, i => j => 1.0+i*j*1.0+j).column(2) must_==
        Vector(3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0, 17.0, 19.0, 21.0)
      build(0, 7, i => j => 1.0+i*j*1.0).column(4) must_==
        Vector(0)
    }
    
    "transpose" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0).transpose
      sut.nCols  must_== 10
      sut.nRows must_== 5
      sut.column(0) must_== Vector(1.0, 1.0, 1.0, 1.0, 1.0)
      sut.column(2) must_== Vector(1.0, 3.0, 5.0, 7.0, 9.0)
      sut.column(9) must_== Vector(1.0, 10.0, 19.0, 28.0, 37.0)
      val sut0 = build(10, 0, i => j => 1.0+i*j*1.0).transpose
      sut0.nCols  must_== 10
      sut0.nRows must_== 0
    }
    
    "copy" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0)
      val copy = sut.copy
      copy(3,4) must_== 13.0
      sut(3,4) = 42.5
      copy(3,4) must_== 13.0
      sut(3,4) must_== 42.5
    }
    
    "+" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0) + build(10, 5, i => j => 1.0-i*j*1.0)
      sut.foreach((i:Int) => (j:Int) => {sut(i,j) must_== 2.0;()})
      ok
    }

    "-" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0) - build(10, 5, i => j => i*j*1.0)
      sut.foreach((i:Int) => (j:Int) => {sut(i,j) must_== 1.0;()})
      ok
    }
  }

  "Matrix of columns" should {
    def build(n: Int, m: Int, f: Int => Int => Double): MutableMatrix =
      Matrix.ofColumns(m, nxm(n, m, f) map Vector.apply)

    "have correct number of columns" in {
      val sut = build(7, 10, i => j => 1.0+i+j)
      sut.nRows must_== 10
      Matrix.ofColumns(10, Array()).nCols must_== 0
      build(7, 0, i => j => 1.0).nCols must_== 7
    }

    "have correct number of rows" in {
      val sut = build(7, 10, i => j => 1.0+i+j)
      sut.nRows must_== 10
      Matrix.ofColumns(10, Array()).nRows must_== 10
      build(7, 0, i => j => 1.0).nRows must_== 0
    }

    "have apply()" in {
      val sut = build(7, 10, i => j => 1.0+i+2*j)
      sut(0, 0) must_== 1.0
      sut(2, 3) must_== 8.0
    }

    "return row" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0+j)
      sut.nRows must_== 5
      sut.nCols must_== 10
      sut.row(2) must_==
        Vector(3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0, 17.0, 19.0, 21.0)
      build(0, 10, i => j => 1.0+i*j*1.0).row(4) must_==
        Vector(0)
    }

    "return column" in {
      val sut: Matrix = build(10, 5, i => j => 1.0 + i * j * 1.0)
      sut.nRows must_== 5
      sut.nCols must_== 10
      sut.column(2) must_==
        Vector(1.0, 3.0, 5.0, 7.0, 9.0)
      build(10, 0, i => j => 1.0+i*j*1.0).column(4) must_==
        Vector(0)
    }

    "transpose" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0).transpose
      sut.nCols  must_== 5
      sut.nRows must_== 10
      sut.row(0) must_== Vector(1.0, 1.0, 1.0, 1.0, 1.0)
      sut.row(2) must_== Vector(1.0, 3.0, 5.0, 7.0, 9.0)
      sut.row(9) must_== Vector(1.0, 10.0, 19.0, 28.0, 37.0)
      val sut0 = build(10, 0, i => j => 1.0+i*j*1.0).transpose
      sut0.nCols  must_== 0
      sut0.nRows must_== 10
    }

    "copy" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0)
      val copy = sut.copy
      copy(3,4) must_== 13.0
      sut(3,4) = 42.5
      sut(3,4) must_== 42.5
      copy(3,4) must_== 13.0
      sut(3,4) must_== 42.5
    }

    "+" in {
      val mx1: Matrix = build(10, 5, i => j => 1.0 + i * j * 1.0)
      val mx2: Matrix = build(10, 5, i => j => 1.0 - i * j * 1.0)
      val sut = mx1 + mx2
      sut.foreach((i:Int) => (j:Int) => {sut(i,j) must_== 2.0; ()})
      ok
    }

    "-" in {
      val mx1: Matrix = build(10, 5, i => j => 1.0 + i * j * 1.0)
      val mx2: Matrix = build(10, 5, i => j =>       i * j * 1.0)
      val sut = mx1 - mx2
      sut.foreach((i:Int) => (j:Int) => {sut(i,j) must_== 1.0; ()})
      ok
    }
  }

  "Matrix with hidden structure" should {
    def build(n: Int, m: Int, f: Int => Int => Double): MutableMatrix = {
      val sut = Matrix(n, m)
      sut.nRows must_== n
      sut.nCols must_== m
      val content: TestMatrix = TestMatrix(n, m, f)
      content.nRows must_== n
      content.nCols must_== m
      sut := content
      sut must_== content

      sut
    }

    "have correct number of rows" in {
      Matrix(2,3).nRows must_== 2
      val sut = build(7, 10, i => j => 1.0+i+j)
      sut.nRows must_== 7
      build(0, 2, i => j => 1.0).nRows must_== 0
      build(7, 0, i => j => 1.0).nRows must_== 7
    }

    "have correct number of columns" in {
      Matrix(2,3).nCols must_== 3
      val sut = build(7, 10, i => j => 1.0+i+j)
      sut.nCols must_== 10
      build(0, 2, i => j => 1.0).nCols must_== 2
      build(7, 0, i => j => 1.0).nCols must_== 0
    }

    "have apply()" in {
      val sut = build(7, 10, i => j => 1.0+i+2*j)
      sut(0, 0) must_== 1.0
      sut(2, 3) must_== 9.0
    }
    
    "contain good data" in {
      val sut = build(2, 3, i => j => i*10.0+j)
      sut(0,0) must_== 0.0
      sut(0,1) must_== 1.0
      sut(0,2) must_== 2.0
      sut(1,0) must_== 10.0
      sut(1,1) must_== 11.0
      sut(1,2) must_== 12.0
    }

    "return row" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0+j)
      sut.nRows must_== 10
      sut.nCols must_== 5
      sut.row(2) must_== Vector(1.0, 4.0, 7.0, 10.0, 13.0)
      build(10, 0, i => j => 1.0+i*j*1.0).row(4) must_== Vector(0)
    }

    "return column" in {
      val sut: Matrix = build(10, 5, i => j => 1.0 + i * j * 1.0)
      sut.nRows must_== 10
      sut.nCols must_== 5
      sut.column(2) must_==
        Vector(1.0, 3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0, 17.0, 19.0)
      build(0, 10, i => j => 1.0+i*j*1.0).column(4) must_==
        Vector(0)
    }

    "transpose" in {
      val sut0: Matrix = build(10, 5, i => j => i*10.0+j)
      sut0(1, 2) must_== 12.0
      sut0(2, 1) must_== 21.0
      sut0.column(0) must_== Vector(0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0)
      sut0.column(1) must_== Vector(1.0, 11.0, 21.0, 31.0, 41.0, 51.0, 61.0, 71.0, 81.0, 91.0)

      val sut = sut0.transpose
      sut.nCols  must_== 10
      sut.nRows must_== 5
      sut.row(0) must_== Vector(0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0)
      sut(1, 2) must_== 21.0
      
      sut.row(1) must_== Vector(1.0, 11.0, 21.0, 31.0, 41.0, 51.0, 61.0, 71.0, 81.0, 91.0)
      sut.row(2) must_== Vector(2.0, 12.0, 22.0, 32.0, 42.0, 52.0, 62.0, 72.0, 82.0, 92.0)
      sut.row(4) must_== Vector(4.0, 14.0, 24.0, 34.0, 44.0, 54.0, 64.0, 74.0, 84.0, 94.0)
      val sutEmpty = build(10, 0, i => j => 1.0+i*j*1.0).transpose
      sutEmpty.nCols  must_== 10
      sutEmpty.nRows must_== 0
    }

    "copy" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0)
      sut(3,4) must_== 13.0
      val copy = sut.copy
      copy(3,4) must_== 13.0
      sut(3,4) = 42.5
      sut(3,4) must_== 42.5
      copy(3,4) must_== 13.0
      sut.row(3).data(4) must_== 42.5
      sut(3,4) must_== 42.5
    }

    "+" in {
      val f1: Int => Int => Double = i => j => 3.0 + 2.0 * j + 11.0*i
      val mx1: Matrix = build(10, 5, f1)
      mx1.nRows must_== 10
      mx1(0, 1) aka mx1.toString must_== 5.0
      f1(0)(5) must_== 13.0

      val mx2: Matrix = build(10, 5, i => j => 1.0 - j - i)
      mx2.nRows must_== 10
      val sut = mx1 + mx2
      sut.nRows must_== 10
      sut.nCols must_== 5
      mx1(0, 1) aka mx1.toString must_== 5.0
      mx2(0, 1) must_== 0.0
      sut.foreach((i:Int) => (j:Int) => {sut(i,j) aka s"@($i,$j)" must_== 4.0 + j + 10*i; ()})
      ok
    }

    "-" in {
      val mx1: Matrix = build(10, 5, i => j => 1.0 + 2*i + 10*j)
      val mx2: Matrix = build(10, 5, i => j =>       i + 3*j)
      mx1(1, 2) must_== 23.0
      mx2(1, 2) must_== 7.0
      val sut = mx1 - mx2
      sut(1, 2) must_== 16.0
      sut.foreach((i:Int) => (j:Int) => {sut(i,j) aka s"@($i,$j)" must_== 1.0 + i + 7*j; ()})
      ok
    }
  }
  
  "Matrix object" should {
    "build diagonal" in {
      diagonal(5, _*5) must_== TestMatrix(5, 5, i => j => if (i == j) i*5.0 else 0.0)
      diagonal(-1, -2, -3) must_== TestMatrix(3, 3, i => j => if (i == j) -i-1 else 0.0)
    }
    
    "build a matrix from partial function" in {
      val sut = new Matrix.OnPartialFunction(3, 10, {case (i:Int, j:Int) if j % (i+1) == 0 => i+j})
      sut must_== Matrix.ofRows(10, Array(
        Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
        Vector(1, 0, 3, 0, 5, 0, 7, 0, 9, 0), 
        Vector(2, 0, 0, 5, 0, 0, 8, 0, 0, 11)))
    }
  }
}
