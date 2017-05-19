package scalakittens.la

import org.specs2.mutable.Specification

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

  case class TestMatrix(h: Int, w: Int, f: Int => Int => Double) extends Matrix {
    override def nRows: Int = h
    override def nCols: Int = w
    val d = nxm(h, w, f)
    private[la] def set(i: Int, j: Int, value: Double): Unit = {
      val row = d(i)
      row(j) = value
    }

    def notImplemented: Nothing = throw new UnsupportedOperationException
    
    override def transpose: Matrix = notImplemented
    override def +(other: Matrix): Matrix = notImplemented
    override def -(other: Matrix): Matrix = notImplemented
    override def copy: Matrix = notImplemented
    override def apply(i: Int, j: Int): Double = {
      val row = d(i)
      row(j)
    }
  }
  
  "Matrix" should {
    "return row" in {
      TestMatrix(10, 5, i => j => 1.0+i*j*1.0).row(2) must_==
      Vector(1.0, 3.0, 5.0, 7.0, 9.0)
      TestMatrix(10, 0, i => j => 1.0+i*j*1.0).row(4) must_==
        Vector(0)
    }

    "return col" in {
      TestMatrix(5, 25, i => j => 0.5+i*j*1.0).column(3) must_==
        Vector(0.5, 3.5, 6.5, 9.5, 12.5)
      TestMatrix(0, 10, i => j => 1.0+i*j*1.0).column(4) must_==
        Vector(0)
    }

    "have foreach()" in {
      val sut = TestMatrix(3, 4, i => j => 0.5+i*j*1.0)
      var log = ""
      sut.foreach(i => j => log = log + i + j)
      log must_== "000102031011121320212223"
    }

    "have +=" in {
      val sut1 = TestMatrix(3, 4, i => j => 0.5+i*j*1.0)
      val sut2 = TestMatrix(3, 4, i => j => 0.5-i*j*1.0)
      sut1 += sut2
      sut1.foreach(i => j => {sut1(i, j) must_== 1.0; ()})
      sut1 must_== TestMatrix(3, 4, i => j => 1.0)
    }
    
    "calculate row_l2" in {
      val sut0 = TestMatrix(1, 2, i => j => 1.0)
      sut0.row(0) must_== Vector(1.0, 1.0)
      sut0.row_l2(0) must_== sqrt(2.0)
      
      val sut = TestMatrix(3, 4, i => j => 0.5+i*10.0 + j)
      val vec = Vector(10.5, 11.5, 12.5, 13.5)
      sut.row(1) must_== vec
      sut.row_l2(1) must_== vec.l2
    }

    "calculate column_l2" in {
      val sut = TestMatrix(3, 4, i => j => 0.5+i*10.0 + j)
      val vec = Vector(1.5, 11.5, 21.5)
      sut.column(1) must_== vec
      sut.column_l2(1) must_== vec.l2
    }
    
    "normalize vertically" in {
      val sut = TestMatrix(3, 4, i => j => 0.5+i*10.0 + j)

      sut.normalizeVertically()

      sut must_== Matrix.ofRows(4, Array(
        Vector(0.021703261485649127,0.06140377126253595,0.09667364890456637,0.12807973746712817),
        Vector(0.45576849119863166,0.4707622463461089,0.48336824452283184,0.49402184451606584),
        Vector(0.8898337209116142,0.8801207214296819,0.8700628401410972,0.8599639515650035)
      ))

      ok
    }
    
    "multiply" in {
      val sut1 = TestMatrix(3, 4, i => j => i*10+j)
      val sut2 = TestMatrix(4, 5, i => j => (i-j + 1000)%2 * 1.0)
      sut1 * sut2 aka s"$sut1\n*\n$sut2" must_== Matrix.ofRows(5, Array(
        Vector(4.0,2.0,4.0,2.0,4.0),
        Vector(24.0,22.0,24.0,22.0,24.0),
        Vector(44.0,42.0,44.0,42.0,44.0)
      ))
    }

    "multiply in place" in {
      val sut1 = TestMatrix(3, 4, i => j => i*10+j)
      val sut2 = TestMatrix(4, 4, i => j => (i-j + 1000)%2 * 1.0)
      sut1 *= sut2 
      sut1 aka s"$sut1\n*\n$sut2" must_== Matrix.ofRows(4, Array(
        Vector(4.0,2.0,4.0,2.0),
        Vector(24.0,22.0,24.0,22.0),
        Vector(44.0,42.0,44.0,42.0)
      ))
      
      sut1 *= sut2
      sut1 must_== Matrix.ofRows(4, Array(
        Vector(4.0,8.0,4.0,8.0),
        Vector(44.0,48.0,44.0,48.0),
        Vector(84.0,88.0,84.0,88.0)
      ))
    }
    
    "multiply by a vector" in {
      val sut = TestMatrix(3, 4, i => j => i*10+j)
      sut * Vector(0, 1, 2, 3) must_== Vector(14, 74, 134)
    }
  }
  
  "Matrix of rows" should {
    def build(n: Int, m: Int, f: Int => Int => Double): Matrix = 
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
      sut.set(3,4,42.5)
      copy(3,4) must_== 13.0
      sut(3,4) must_== 42.5
    }
    
    "+" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0) + build(10, 5, i => j => 1.0-i*j*1.0)
      sut.foreach(i => j => {sut(i,j) must_== 2.0;()})
      ok
    }

    "-" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0) - build(10, 5, i => j => i*j*1.0)
      sut.foreach(i => j => {sut(i,j) must_== 1.0;()})
      ok
    }

    "+=" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0)
      val copy = sut.copy
      sut += build(10, 5, i => j => 1.0-i*j*1.0)
      sut.foreach(i => j => {sut(i,j) must_== 2.0; ()})
      copy(4,3) must_== 13.0
      ok
    }

    "-=" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0)
      val copy = sut.copy
      sut -= build(10, 5, i => j => i*j*1.0)
      sut.foreach(i => j => {sut(i,j) must_== 1.0; ()})
      copy(4,3) must_== 13.0
      ok
    }
  }

  "Matrix of columns" should {
    def build(n: Int, m: Int, f: Int => Int => Double): Matrix =
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
      sut.set(3,4,42.5)
      sut(3,4) must_== 42.5
      copy(3,4) must_== 13.0
      sut(3,4) must_== 42.5
    }

    "+" in {
      val mx1: Matrix = build(10, 5, i => j => 1.0 + i * j * 1.0)
      val mx2: Matrix = build(10, 5, i => j => 1.0 - i * j * 1.0)
      val sut = mx1 + mx2
      sut.foreach(i => j => {sut(i,j) must_== 2.0; ()})
      ok
    }

    "-" in {
      val mx1: Matrix = build(10, 5, i => j => 1.0 + i * j * 1.0)
      val mx2: Matrix = build(10, 5, i => j =>       i * j * 1.0)
      val sut = mx1 - mx2
      sut.foreach(i => j => {sut(i,j) must_== 1.0; ()})
      ok
    }

    "+=" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0)
      val copy = sut.copy
      sut += build(10, 5, i => j => 1.0-i*j*1.0)
      sut.foreach(i => j => {sut(i,j) must_== 2.0; ()})
      copy(4,3) must_== 13.0
      ok
    }

    "-=" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0)
      val copy = sut.copy
      sut -= build(10, 5, i => j => i*j*1.0)
      sut.foreach(i => j => {sut(i,j) must_== 1.0; ()})
      copy(4,3) must_== 13.0
      ok
    }
  }

  "Matrix with hidden structure" should {
    def build(n: Int, m: Int, f: Int => Int => Double): Matrix = {
      val sut = Matrix(n, m)
      sut.nRows must_== n
      sut.nCols must_== m
      val content: TestMatrix = TestMatrix(n, m, f)
      content.nRows must_== n
      content.nCols must_== m
      sut := content
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
      sut.set(3,4, 42.5)
      sut(3,4) must_== 42.5
      copy(3,4) must_== 13.0
      sut.row(3).data(4) must_== 42.5
      sut(3,4) must_== 42.5
    }

    "+" in {
      val mx1: Matrix = build(10, 5, i => j => 1.0 + i * j * 1.0)
      val mx2: Matrix = build(10, 5, i => j => 1.0 - i * j * 1.0)
      val sut = mx1 + mx2
      sut.foreach(i => j => {sut(i,j) must_== 2.0; ()})
      ok
    }

    "-" in {
      val mx1: Matrix = build(10, 5, i => j => 1.0 + i * j * 1.0)
      val mx2: Matrix = build(10, 5, i => j =>       i * j * 1.0)
      val sut = mx1 - mx2
      sut.foreach(i => j => {sut(i,j) must_== 1.0; ()})
      ok
    }

    "+=" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0)
      sut(4,3) must_== 13.0
      val copy = sut.copy
      copy(4,3) must_== 13.0
      sut += build(10, 5, i => j => 1.0-i*j*1.0)
      sut.foreach(i => j => {sut(i,j) must_== 2.0;()})
      copy(4,3) must_== 13.0
      ok
    }

    "-=" in {
      val sut = build(10, 5, i => j => 1.0+i*j*1.0)
      sut(4,3) must_== 13.0
      val copy = sut.copy
      copy(4,3) must_== 13.0
      sut -= build(10, 5, i => j => i*j*1.0)
      sut.foreach(i => j => {sut(i,j) must_== 1.0;()})
      copy(4,3) must_== 13.0
      ok
    }
  }
  
  "Matrix object" should {
    "calculate covariance" in {
      val vectors = Array(Vector(1.0, 3.0, -.5, 1.0), Vector(2.0, 6.0, -1.0, 0.0), Vector(3.0, 9.0, -1.5, 1.0))
      val sut = Matrix.covariance(vectors)


      val expected: Matrix = Matrix.ofRows(4, Array(
        Vector(1.0, 3.0, -0.5, 0.0),
        Vector(3.0, 9.0, -1.5, 0.0),
        Vector(-0.5, -1.5, 0.25, 0.0),
        Vector(0.0, 0.0, 0.0, 0.33333333333333337)
      ))
      
      for {
        i <- 0 until 4
      } sut.row(i) aka s"@$i" must_== expected.row(i)
      
      sut must_== expected
    }
  }
}
