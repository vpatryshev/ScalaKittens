package scalakittens.la

import org.specs2.mutable.Specification

import scalakittens.la.Matrix.OnFunction

/**
  * Created by vpatryshev on 5/7/17.
  */
class MatrixTest extends Specification {
  import Spaces._
  
  private def nxm(n: Int, m: Int, f: Int ⇒ Int ⇒ Double) = {
    ((0 until n) map { i ⇒
      ((0 until m) map { j ⇒
        f(i)(j)
      }).toArray
    }).toArray
  }

  case class TestMatrix[Dom <: VectorSpace, Codom <: VectorSpace](
      override val domain: Dom,
      override val codomain: Codom, f: Int ⇒ Int ⇒ Double) 
    extends MutableMatrix[Dom, Codom](domain, codomain) {
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
      TestMatrix(R5, R10, i ⇒ j ⇒ 1.0 + i * j * 1.0).row(2) ===
        R5.Vector(1.0, 3.0, 5.0, 7.0, 9.0)
      TestMatrix(R0, R10, i ⇒ j ⇒ 1.0 + i * j * 1.0).row(4) ===
        R0.Vector()
    }

    "return col" in {
      TestMatrix(R25, R5, i ⇒ j ⇒ 0.5 + i * j * 1.0).column(3) ===
        R5.Vector(0.5, 3.5, 6.5, 9.5, 12.5)
      TestMatrix(R10, R0, i ⇒ j ⇒ 1.0 + i * j * 1.0).column(4) ===
        R0.Vector()
    }

    "have foreach()" in {
      val sut = TestMatrix(R4, R3, i ⇒ j ⇒ 0.5 + i * j * 1.0)
      var log = ""
      sut.foreach((i:Int) ⇒ (j:Int) ⇒ log = log + i + j)
      log === "000102031011121320212223"
    }

    "produce all elements" in {
      val data = 
        Array(4.0, 2.0, 4.0, 2.0, 4.0,
          24.0, 22.0, 24.0, 22.0, 24.0,
          44.0, 42.0, 44.0, 42.0, 44.0)
      
      val sut = Matrix(R5, R3, data)
      
      sut.allElements.toList === data.toList
    }
    
    "multiply" in {
      val sut1 = TestMatrix[R4.type, R3.type](R4, R3, i ⇒ j ⇒ i * 10 + j)
      val sut2 = TestMatrix[R5.type, R4.type](R5, R4, i ⇒ j ⇒ (i - j + 1000) % 2 * 1.0)

      val expected = Matrix(R5, R3, 
        Array(4.0, 2.0, 4.0, 2.0, 4.0,
             24.0, 22.0, 24.0, 22.0, 24.0,
             44.0, 42.0, 44.0, 42.0, 44.0)
      )

      val data = new Array[Double](3 * 5)
      for {
        i ← 0 until 3
        j ← 0 until 5
      } data(i * 5 + j) = (0 until 4) map (k ⇒ sut1(i, k) * sut2(k, j)) sum

      data === Array(4.0, 2.0, 4.0, 2.0, 4.0, 24.0, 22.0, 24.0, 22.0, 24.0, 44.0, 42.0, 44.0, 42.0, 44.0)

      val product = Matrix(R5, R3, data)

      product === expected

//      R4.mult(sut1, sut2) aka s"$sut1\n*\n$sut2" must_== expected
    }

    "multiply by a vector" in {
      val sut = TestMatrix(R4, R3, i ⇒ j ⇒ i * 10 + j)
      (sut * sut.domain.Vector(0, 1, 2, 3)) === R3.Vector(14, 74, 134)
    }

    "multiply by an immutable vector" in {
      val sut = TestMatrix(R4, R3, i ⇒ j ⇒ i * 10 + j)
      val vec = sut.domain.OnFunction(i ⇒ 1.0*i)
      val actual = sut * vec
      actual === R3.Vector(14, 74, 134)
    }
    
    "not compare with garbage" in {
      val sut: Any = TestMatrix(R4, R3, i ⇒ j ⇒ i * 10 + j)
      
      sut.equals("nothing") must beFalse
    }

    "not compare with a matrix of different dimensions" in {
      val sut: Any = TestMatrix(R4, R3, i ⇒ j ⇒ i * 10 + j)
      sut.equals(TestMatrix(R4, R4, i ⇒ j ⇒ i * 10 + j)) must beFalse
      sut.equals(TestMatrix(R3, R3, i ⇒ j ⇒ i * 10 + j)) must beFalse
      sut.equals(TestMatrix(R3, R4, i ⇒ j ⇒ i * 10 + j)) must beFalse
    }
    
    "get built" in {
      val sut = Matrix.build(R3, R2)
      sut.domain === R3
      sut.codomain === R2
      sut(1,2) = 3.14
      sut(1,2) === 3.14
    }
  }

  "Matrix with hidden structure" should {
    def build(n: VectorSpace, m: VectorSpace, f: Int ⇒ Int ⇒ Double): MutableMatrix[n.type, m.type] = {
      val sut: MutableMatrix[n.type, m.type] = Matrix(n, m)
      sut.nRows === m.dim
      sut.nCols === n.dim
      sut.domain === n
      sut.codomain === m
      val content: MutableMatrix[n.type, m.type] = TestMatrix(n, m, f)
      content.nRows === m.dim
      content.nCols === n.dim
      content.domain === n
      content.codomain === m
      sut := content
      sut === content

      sut
    }

    "have correct number of rows" in {
      Matrix(R3, R2).nRows === 2
      val sut = build(R10, R7, i ⇒ j ⇒ 1.0+i+j)
      sut.nRows === 7
      build(R2, R0, i ⇒ j ⇒ 1.0).nRows === 0
      build(R0, R7, i ⇒ j ⇒ 1.0).nRows === 7
    }

    "have correct number of columns" in {
      Matrix(R3, R2).nCols === 3
      val sut = build(R7, R10, i ⇒ j ⇒ 1.0+i+j)
      sut.nCols === 7
      build(R2, R0, i ⇒ j ⇒ 1.0).nCols === 2
      build(R0, R7, i ⇒ j ⇒ 1.0).nCols === 0
    }

    "have apply()" in {
      val sut = build(R10, R7, i ⇒ j ⇒ 1.0+i+2*j)
      sut(0, 0) === 1.0
      sut(2, 3) === 9.0
    }
    
    "contain good data" in {
      val sut = build(R3, R2, i ⇒ j ⇒ i*10.0+j)
      sut(0,0) === 0.0
      sut(0,1) === 1.0
      sut(0,2) === 2.0
      sut(1,0) === 10.0
      sut(1,1) === 11.0
      sut(1,2) === 12.0
    }

    "return row" in {
      val sut = build(R5, R10, i ⇒ j ⇒ 1.0+i*j*1.0+j)
      sut.nRows === 10
      sut.nCols === 5
      sut.row(2) === R5.Vector(1.0, 4.0, 7.0, 10.0, 13.0)
    }

    "return column" in {
      val sut = build(R5, R10, i ⇒ j ⇒ 1.0 + i * j * 1.0)
      sut.nRows === 10
      sut.nCols === 5
      sut.domain === R5
      sut.codomain === R10
      sut.column(2) ===
        R10.Vector(1.0, 3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0, 17.0, 19.0)
      build(R10, R0, i ⇒ j ⇒ 1.0+i*j*1.0).column(4) ===
        R0.Vector()
    }

    "transpose" in {
      val sut0 = build(R5, R10, i ⇒ j ⇒ i*10.0+j)
      sut0(1, 2) === 12.0
      sut0(2, 1) === 21.0
      sut0.column(0) === R10.Vector(0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0)
      sut0.column(1) === R10.Vector(1.0, 11.0, 21.0, 31.0, 41.0, 51.0, 61.0, 71.0, 81.0, 91.0)

      val sut = sut0.transpose
      sut.nCols  === 10
      sut.nRows === 5
      sut.domain === R10
      sut.codomain === R5
      sut.row(0) === R10.Vector(0.0, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0)
      sut(1, 2) === 21.0
      
      sut.row(1) === R10.Vector(1.0, 11.0, 21.0, 31.0, 41.0, 51.0, 61.0, 71.0, 81.0, 91.0)
      sut.row(2) === R10.Vector(2.0, 12.0, 22.0, 32.0, 42.0, 52.0, 62.0, 72.0, 82.0, 92.0)
      sut.row(4) === R10.Vector(4.0, 14.0, 24.0, 34.0, 44.0, 54.0, 64.0, 74.0, 84.0, 94.0)
      val sutEmpty = build(R0, R10, i ⇒ j ⇒ 1.0+i*j*1.0).transpose
      sutEmpty.nCols  === 10
      sutEmpty.nRows === 0
    }

    "copy" in {
      val sut = build(R10, R5, i ⇒ j ⇒ 1.0+i*j*1.0)
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
      val f1: Int ⇒ Int ⇒ Double = i ⇒ j ⇒ 3.0 + 2.0 * j + 11.0*i
      val mx1 = build(R5, R10, f1)
      mx1.nRows === 10
      mx1(0, 1) aka mx1.toString must_== 5.0
      f1(0)(5) === 13.0

      val mx2 = build(R5, R10, i ⇒ j ⇒ 1.0 - j - i)
      mx2.nRows === 10
      val sut = mx1 + mx2
      sut.nRows === 10
      sut.nCols === 5
      mx1(0, 1) aka mx1.toString must_== 5.0
      mx2(0, 1) === 0.0
      sut.foreach((i:Int) ⇒ (j:Int) ⇒ {
        val xij = sut(i, j)
        xij aka s"@($i,$j)" must_== 4.0 + j + 10*i; ()})
      ok
    }

    "-" in {
      val mx1 = build(R5, R10, i ⇒ j ⇒ 1.0 + 2*i + 10*j)
      val mx2 = build(R5, R10, i ⇒ j ⇒       i + 3*j)
      mx1(1, 2) === 23.0
      mx2(1, 2) === 7.0
      val sut = mx1 - mx2
      sut(1, 2) === 16.0
      sut.foreach((i:Int) ⇒ (j:Int) ⇒ {
        val xij = sut(i, j)
        val expected: Double = 1.0 + i + 7*j
        xij aka s"@($i,$j)" must_== expected
        ()})
      ok
    }
  }
  
  "Matrix object" should {
    "build diagonal" in {
      R5.diagonalMatrix(_*5) === TestMatrix(R5, R5, i ⇒ j ⇒ if (i == j) i*5.0 else 0.0)
      R3.diagonalMatrix(-1, -2, -3) === TestMatrix(R3, R3, i ⇒ j ⇒ if (i == j) -i-1 else 0.0)
    }
    
    "build a matrix from partial function" in {
      val sut = new Matrix.OnPartialFunction(R10, R3, {case (i:Int, j:Int) if j % (i+1) == 0 ⇒ i+j})
      sut === Matrix(R10, R3, Array(
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 
        1, 0, 3, 0, 5, 0, 7, 0, 9, 0, 
        2, 0, 0, 5, 0, 0, 8, 0, 0, 11))
    }

    "build a triangular matrix" in {
      val sut0 = R2.squareMatrix((i, j) ⇒ 1 + i * i + j * j).triangle
      
      Array(sut0(0, 0), sut0(1, 0), sut0(0, 1), sut0(1, 1)) must_== Array(1, 2, 2, 3)

      val source = R10.squareMatrix((i, j)  ⇒ 1 + i*i+j*j)

      val sut = source.triangle
      sut(5,5) must_== 51.0
      sut(5,9) must_== 107.0
      val sucp = sut.copy
      sucp(5,5) must_== 51.0
      sucp(5,9) must_== 107.0
      sucp must_== source
      source.transpose.copy must_== sucp
      source must_== sut
      source.transpose must_== sut
    }
    
    "properly check compatibility" in {
      val m1: Matrix[R2.type, R3.type] = 
        new OnFunction[R2.type, R3.type](R2, R3, (i: Int, j: Int) ⇒ 1 + i * i + j * j)

      val m2: Matrix[R5.type, R6.type] =
        new OnFunction[R5.type, R6.type](R5, R6, (i: Int, j: Int) ⇒ 1 - i * i - j * j)

      // the following line should not compile
      // val badMatrix: Matrix[VectorSpace, VectorSpace] = m1 + m2
      
      ok
    }
    
  }
}
