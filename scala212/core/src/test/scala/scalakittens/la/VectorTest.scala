package scalakittens.la

import org.specs2.mutable.Specification

import scalakittens.la.Norm.l2

/**
  * Created by vpatryshev on 5/7/17.
  */
class VectorTest extends Specification {
  import math._
  import Spaces._

  "VectorSpace" should {

    "build unit cube" in {
      val vectors = R3.Vector(0,1,2)::R3.Vector(0,2,4)::R3.Vector(-2,3,10)::Nil
      val unitCube = R3.unitCube(vectors)
      val z = unitCube(R3.Zero)
      val ucs = unitCube.toString
      (z aka ucs) must_== R3.Vector(1,-0.5,-0.25)
      unitCube(R3.Vector(-1, 0, 0)) === R3.Vector(0.5,-0.5,-0.25)
      unitCube(R3.Vector(0,1,0)) === R3.Vector(1,0,-0.25)
      unitCube(R3.Vector(0,0,1)) === R3.Vector(1.0,-0.5,-0.125)
    }

    "inject into unit cube" in {
      val vectors = R3.Vector(0,1,2)::R3.Vector(0,2,4)::R3.Vector(-2,3,10)::Nil
      val normalized: List[R3.Vector] = R3.toUnitCube(vectors).toList
      normalized === R3.Vector(1,0,0)::R3.Vector(1,0.5,0.25)::R3.Vector(0,1,1)::Nil

    }
    
    "build orthonormal basis 2x2" in {
      val vecs = R2.buildOrthonormalBasis(R2.Vector(3, 4))
      for {i <- 0 until 2; j <- 0 until 2} {
        val prod = vecs(i)*vecs(j)
        val expected = if (i == j) 1.0 else 0.0
        abs(prod - expected) < 0.0001 aka s"@($i, $j): ${vecs(i)}*${vecs(j)} = $prod != $expected " must beTrue
      }

      val v0 = R2.Vector(0.6000000000000001, 0.8)
      val v1 = R2.Vector(-0.8000000000000001, 0.5999999999999999)
      vecs(0) === v0
      vecs(1) === v1
      vecs.toList === List(v0, v1)
      ok
    }

    "build orthonormal basis 3x3" in {
      val vecs = R3.buildOrthonormalBasis(R3.Vector(3, 5, 4))
      for {i <- 0 until 3; j <- 0 until 3} {
        val prod = vecs(i)*vecs(j)
        val expected = if (i == j) 1.0 else 0.0
        abs(prod - expected) < 0.0001 aka s"@($i, $j): ${vecs(i)}*${vecs(j)} = $prod != $expected " must beTrue
      }
      ok
    }

    "build orthonormal basis 4x4" in {
      val vecs = R4.buildOrthonormalBasis(R4.Vector(17, -1, 3, 4))
      for {i <- 0 until 4; j <- 0 until 4} {
        val prod = vecs(i)*vecs(j)
        val expected = if (i == j) 1.0 else 0.0
        abs(prod - expected) < 0.0001 aka s"@($i, $j): ${vecs(i)}*${vecs(j)} = $prod != $expected " must beTrue
      }
      ok
    }

    "produce a vector from function" in {
      val sut1 = new R0.OnFunction(i => 42)
      sut1 === R0.Vector()
      new Spaces.R6.OnFunction(i => (i*sqrt(i)).toInt) === R6.Vector(0, 1, 2, 5, 8, 11)
    }

    "produce a unit vector" in {
      R1.unit(0) === R1.Vector(1.0)
      for (i <- 0 until 10) {
        val sut = R10.unit(i)
        for (j <- 0 until 10) {
          sut(j) === (if (i == j) 1.0 else 0.0)
        }
      }
      ok
    }

    "compile with subspaces" in {
      val space0 = VectorSpace(3)
      def recurse(s: VectorSpace)(v: s.Vector): Unit = {
        val ignoreme = if (s.dim == 0) v else {
          val v1 = s.projectToHyperplane(v) * 2
          recurse(s.hyperplane)(v1)
        }
        ignoreme mustNotEqual null
        ()
      }
      val sut = space0.const(3)
      recurse(space0)(sut)
      ok
    }

    "read vectors" in {
      val sut = R3.Vector(math.Pi, math.E, Double.MinPositiveValue)
      val s = sut.toString
      R3.readVector(s) === Some(sut)
      R2.readVector(s) === None
      R4.readVector(s) === None
      R3.readVector("this is not a vector") === None
    }

    "build square matrix" in {
      val sm: R3.SquareMatrix = R3.squareMatrix((i, j) => 1.0 / (i + j + 1))
      sm(0, 2) === sm(2, 0)
      sm(0, 1) === 0.5
    }


    val alpha = Pi / 4
    val beta = Pi / 3

    val sampleUnitaryMatrix_3x3: R3.UnitaryMatrix = {
      R3.unitaryMatrix(
        Array(R3.Vector(cos(alpha) * cos(beta), cos(alpha) * sin(beta), sin(alpha)),
          R3.Vector(-sin(alpha) * cos(beta), -sin(alpha) * sin(beta), cos(alpha)),
          R3.Vector(sin(beta), -cos(beta), 0)
        ))
    }

    val sampleUnitaryMatrix_2x2: R2.UnitaryMatrix = R2.unitaryMatrix(
      Array(R2.Vector(cos(beta), sin(beta)),
        R2.Vector(-sin(beta), cos(beta))
      ))

    "check unitariness" in {
      val u0 = R2.unitaryMatrix(Array(R2.Vector(0, 1), R2.Vector(1, 0)))
      u0.isUnitary(0) aka s"delta = ${l2(u0 * u0.transpose - R2.UnitMatrix)}" must beTrue

      sampleUnitaryMatrix_2x2.isUnitary(0.001) aka s"delta = ${l2(sampleUnitaryMatrix_2x2 * sampleUnitaryMatrix_2x2.transpose - R2.UnitMatrix)}" must beTrue

      sampleUnitaryMatrix_3x3.isUnitary(0.001) aka s"delta = ${l2(sampleUnitaryMatrix_3x3 * sampleUnitaryMatrix_3x3.transpose - R3.UnitMatrix)}" must beTrue
    }

    "project matrix to hyperplane" in {
      val m = R3.squareMatrix(
        Array(
          1.1250000000000009, 3.6806079660838646, 1.2500000000000002,
          3.6806079660838655, 5.375000000000001, 2.165063509461097,
          1.2500000000000002, 2.1650635094610973, 7.5))
      val sut = m.projectToHyperplane(sampleUnitaryMatrix_3x3)
      l2(sut - R3.hyperplane.diagonalMatrix(5, -1)) < 0.0001 aka sut.toString must beTrue
    }

    "project vector to hyperplane" in {
      val v = R3.Vector(111.0, 222.0, 333.0)
      val sut = R3.projectToHyperplane(v)
      sut.length must_== 2
      sut must_== R2.Vector(222.0, 333.0)
    }

    "inject vector from hyperplane" in {
      val v = R3.Vector(111.0, 222.0, 333.0)
      val sut = R4.injectFromHyperplane(v)
      sut.length must_== 4
      sut must_== R4.Vector(0.0, 111.0, 222.0, 333.0)
      R4.injectFromHyperplane(R2.Vector(1.1, 2.2)) must throwA[Exception]
    }
    
    "rotate square matrix" in {

      val m0 = R3.diagonalMatrix(10, 5, -1)

      val m1 = m0 rotate sampleUnitaryMatrix_3x3

      val m2 = m1 rotate sampleUnitaryMatrix_3x3.transpose

      l2(m2 - m0) < 0.00001 aka m2.toString must beTrue

      m1 === Matrix(R3, R3,
        Array(
          1.1250000000000009, 3.6806079660838646, 1.2500000000000002,
          3.6806079660838655, 5.375000000000001, 2.165063509461097,
          1.2500000000000002, 2.1650635094610973, 7.5))
    }
  }

  "Vector" should {
    "exist" in {
      ok
    }
    
    "know its length" in {
      val sut1 = R0.Vector(new Array[Double](0))
      sut1.length === 0
      R1.Vector(0.0).length === 1
      R2.Vector(1.5, 2.25).length === 2
    }

    "be defined at proper indexes" in {
      val sut = R3.Vector(5, 7, 42)
      for (i <- -1 until 5) {
        sut.isDefinedAt(i) === (i >= 0 && i < 3)
      }
      
      ok
    }
    
    "fail on wrong length" in {
      R0.Vector(42.0) must throwA[Exception]
      R1.Vector() must throwA[Exception]
      R1.Vector(2.0, 42.0) must throwA[Exception]
      R2.Vector(6.5) must throwA[Exception]
    }

    "check validity" in {
      R2.Vector(2.0, 42.0).isValid must beTrue
      R7.Vector(1.0, Double.NaN, 2.0, Double.NegativeInfinity, Double.MaxValue, Double.PositiveInfinity, Double.MinPositiveValue).isValid must beFalse
      R5.Vector(1.0,  2.0, Double.MaxValue, Double.MinValue, Double.MinPositiveValue).isValid must beTrue
      R3.Vector(1.0, Double.NaN, 2.0).isValid must beFalse
      R3.Vector(2.0, Double.NegativeInfinity, Double.MaxValue).isValid must beFalse
      R3.Vector(1.0, 2.0, Double.PositiveInfinity).isValid must beFalse
    }

    "have apply()" in {
      val sut = R2.Vector(1.5, 2.25)
      sut(0) === 1.5
      sut(1) === 2.25
    }
    
    "have update()" in {
      val sut =R2.Vector(0, 0).copy
      sut(0) = 1.5
      sut(0) === 1.5
      sut(1) === 0.0
      sut(1) = 2.25
      sut(0) === 1.5
      sut(1) === 2.25
    }

    "have foreach()" in {
      val sut = R2.Vector(1.5, 2.25)
      var s = -3.75
      sut foreach (s += _)
      s === 0.0
    }

    "have forall()" in {
      R0.Vector() forall (_ == Pi) must beTrue
      R3.Vector(Pi, Pi, Pi) forall (_ == Pi) must beTrue
      R3.Vector(Pi, 0, Pi) forall (_ == Pi) must beFalse
    }

    "have exists()" in {
      R0.Vector() exists (Pi == _*1.1) must beFalse
      R3.Vector(Pi, Pi, Pi) exists (x => x*x == Pi*Pi) must beTrue
      R3.Vector(2.0, Pi, 1.0) exists (x => x*x == Pi*Pi) must beTrue
      R3.Vector(2.0, 2.0, 1.0) exists (x => x*x == Pi*Pi) must beFalse
    }

    "have map()" in {
      R0.Vector() .map ((x: Double) => "oi-vei") .toList  === Nil

      R3.Vector(3.0, 2.0, 1.0) .map (":) " + _) .toList === List(":) 3.0", ":) 2.0", ":) 1.0")
    }

    "have /:" in {
      (":)" /: R0.Vector())(_+_) === ":)"
      (":)" /: R1.Vector(0))(_+_) === ":)0.0" // this sucks, but it's life... or is it?
      (-1.0 /: R3.Vector(3.0, 2.0, 1.0))(_+_) === 5.0
    }

    "have copy" in {
      R0.Vector().copy.length === 0
      val sut: R3.MutableVector = R3.Vector(Pi, E, 42.0).copy
      val copy = sut.copy
      sut *= 2.0
      copy === R3.Vector(Pi, E, 42.0)
      sut *= 0.5
      copy *= 0.0
      sut === R3.Vector(Pi, E, 42.0)
    }

    "multiply by scalar" in {
      val sut = R2.Vector(1.5, 2.25) * 3.0
      sut === R2.Vector(4.5, 6.75)
    }

    "divide by scalar" in {
      val sut = R2.Vector(1.5, 2.25) / 2
      sut === R2.Vector(0.75, 1.125)
    }

    "multiply by scalar in place" in {
      val sut: R2.MutableVector = R2.Vector(1.5, 2.25).copy
      sut *= 3.0
      sut === R2.Vector(4.5, 6.75)
    }

    "divide by scalar in place" in {
      val sut: R2.MutableVector = R2.Vector(1.5, 2.25)
      sut /= 2
      sut === R2.Vector(0.75, 1.125)
    }

    "multiply by another Vec" in {
      val sut = R2.Vector(1.5, 2.25) * R2.Vector(2.0, -3.0)
      sut === -3.75
    }

    "add another vec" in {
      val sut = R2.Vector(3.25, -7.5) + R2.Vector(6.0, 4.25)
      sut === R2.Vector(9.25, -3.25)
    }

    "add another vec to mutable: it changes under our hands" in {
      val vec1 = R2.Vector(3.25, -7.5).copy
      val sut1 = vec1 + R2.Vector(6.0, 4.25)
      val sut2 = R2.Vector(6.0, 4.25) + vec1
      vec1(0) = 0.0
      sut1 === R2.Vector(6, -3.25)
      sut2 === R2.Vector(6, -3.25)
    }

    "add another vec in place" in {
      val sut: R2.MutableVector = R2.Vector(3.25, -7.5).copy
      sut += R2.Vector(6.0, 4.25)
      sut === R2.Vector(9.25, -3.25)
    }

    "subtract another vec" in {
      val sut = R2.Vector(3.25, -7.5) - R2.Vector(6.0, 4.25)
      sut === R2.Vector(-2.75, -11.75)
    }

    "subtract another vec where one is mutable: it changes under our hands" in {
      val vec1: R2.MutableVector = R2.Vector(3.25, -7.5).copy
      val vec2: R2.MutableVector = R2.Vector(6.0, 4.25).copy
      vec1(0) = 0
      vec2(0) = 0
      val sut = vec1 - vec2
      sut === R2.Vector(0, -11.75)
    }

    "subtract another vec in place" in {
      val sut: R2.MutableVector = R2.Vector(3.25, -7.5).copy
      sut -= R2.Vector(6.0, 4.25)
      sut === R2.Vector(-2.75, -11.75)
    }

    "get nudged by another vec" in {
      val sut: R2.MutableVector = R2.Vector(3.25, -7.5).copy
      sut.nudge(R2.Vector(6.0, 4.75), 2.0)
      sut === R2.Vector(15.25, 2.0)
    }

    "project" in {
      R2.project(R2.Vector(1, 1), R2.Vector(2, -2)) === R2.Vector(0.0, 0.0)
      R2.project(R2.Vector(1, 1), R2.Vector(0, -2)) === R2.Vector(-1.414213562373095, -1.414213562373095)
    }

    "produce infimum" in {
      val sut = R2.Vector(7.25, -7.5) inf R2.Vector(6.0, 4.25)
      sut === R2.Vector(6.0, -7.5)
      (R2.Vector(7.25, -7.5) inf R2.Vector(6.0, Double.MinPositiveValue)) === R2.Vector(6.0, -7.5)
      (R2.Vector(Double.MaxValue, -7.5) inf R2.Vector(6.0, Double.MinValue)) === R2.Vector(6.0, Double.MinValue)
      (R2.Vector(Double.MaxValue, Double.MinValue) inf R2.Vector(Double.MaxValue, Double.MinValue)) === R2.Vector(Double.MaxValue, Double.MinValue)
      (R2.Vector(Double.MinValue, Double.MaxValue) inf R2.Vector(Double.MaxValue, Double.MinValue)) === R2.Vector(Double.MinValue, Double.MinValue)
    }

    "produce supremum" in {
      val sut = R2.Vector(7.25, -7.5) sup R2.Vector(6.0, 4.25)
      sut === R2.Vector(7.25, 4.25)
      (R2.Vector(7.25, -7.5) sup R2.Vector(6.0, Double.MinPositiveValue)) === R2.Vector(7.25, Double.MinPositiveValue)
      (R2.Vector(Double.MaxValue, -7.5) sup R2.Vector(6.0, Double.MinValue)) === R2.Vector(Double.MaxValue, -7.5)
      (R2.Vector(Double.MaxValue, Double.MinValue) sup R2.Vector(Double.MaxValue, Double.MinValue)) === R2.Vector(Double.MaxValue, Double.MinValue)
      (R2.Vector(Double.MinValue, Double.MaxValue) sup R2.Vector(Double.MaxValue, Double.MinValue)) === R2.Vector(Double.MaxValue, Double.MaxValue)
    }

    "normalize" in {
      R0.Vector().normalize(l2) must_== R0.Vector()
      R1.Vector(0).normalize(l2) must_== R1.Vector(0)
      R3.Vector(0, 0, 0).normalize(l2) must_== R3.Vector(0, 0, 0)
      R2.Vector(-sqrt(2), sqrt(2)).normalize(l2) must_== R2.Vector(-sqrt(0.5), sqrt(0.5))
    }
  }

  "Factories" should {
    "be able to produce empty vector" in {
      R0.Zero === R0.Vector()
    }
    
    "build zero vector" in {
      R3.Zero === R3.Vector(0.0, 0.0, 0.0)
    }

    "build 'random' vector" in {
      val sut = R3.RandomCube(314159)()
      sut forall (abs(_) <= 1.0)
      
      ok
    }
    
    "Produce vectors on a sphere" in {
      val factory = R3.RandomSphere(1500000000L)  
      for (i <- 0 until 1000) {
        math.abs(Norm.l2(factory()) - 1.0) < 1.0E-10 must beTrue
      }
      ok
    }
  }

  "Basis" should {
    val sampleCenter = R3.Vector(1, 1, 1)

    val alpha = Pi / 4
    val beta = Pi / 3

    val sampleBasisVectors = Array(
      R3.Vector(cos(alpha) * cos(beta), cos(alpha) * sin(beta), sin(alpha)),
      R3.Vector(-sin(alpha) * cos(beta), -sin(alpha) * sin(beta), cos(alpha)),
      R3.Vector(sin(beta), -cos(beta), 0)
    )

    val sampleUnitaryMatrix_3x3: R3.UnitaryMatrix = {
      R3.unitaryMatrix(
        sampleBasisVectors)
    }
    
    "get built out of mutable vectors" in {
      val basis = R3.Basis(sampleCenter, sampleBasisVectors)
      basis === new R3.Basis(sampleCenter, sampleUnitaryMatrix_3x3)
    }

    "get built out of plain vectors" in {
      val vectors: Array[R3.Vector] = sampleBasisVectors map (_.asInstanceOf[R3.Vector])
      
      val basis = R3.Basis(sampleCenter, vectors)
      
      basis === new R3.Basis(sampleCenter, sampleUnitaryMatrix_3x3)
    }

    "get built out of basis vectors" in {

      val basis = R3.Basis(sampleBasisVectors)

      basis === new R3.Basis(R3.Zero, sampleUnitaryMatrix_3x3)
    }

    "transform forth and back" in {

      val v0 = R3.Zero
      val v1: R3.Vector = R3.Vector(-0.8660254037844386, 0.5, -1.414213562373095)

      val sut = new R3.Basis(sampleCenter, sampleUnitaryMatrix_3x3)

      sut(sampleCenter) === v0
      sut.unapply(v0) === sampleCenter
      sut(v0) === v1
      val v2 = sut.unapply(v1)
      Norm.l2(v2 - v0) < 0.000001 aka v2.toString must beTrue
      val v3 = R3.Vector(2.6730326074756157, 0.7411809548974794, 1.3660254037844384)
      sut.unapply(sampleCenter) === v3
    }
  }
  
  "ColumnMatrix" should {
    val sut = new R3.ColumnMatrix[R2.type](R2, R3.Vector(11, 12, 13)::R3.Vector(21,22,23)::Nil)

    "transpose" in {
      sut.transpose.row(0) === R3.Vector(11, 12, 13)
      sut.transpose.row(1) === R3.Vector(21,22,23)
    }

    "apply" in {
      for {i <- 0 until 2; j <- 0 until 3} {
        val x = sut(j, i)
        x === (i*10 + j + 11).toDouble
      }
      ok
    }
  }

  "RowMatrix" should {
    val sut = new R3.RowMatrix[R2.type](R2, R3.Vector(11, 12, 13)::R3.Vector(21,22,23)::Nil)

    "know its dimensions" in {
      sut.nRows === 2
      sut.nCols === 3
      sut.domain === R3
      sut.row(1) === R3.Vector(21,22,23) 
    }

    "apply" in {
      for {i <- 0 until 2; j <- 0 until 3} {
        val x = sut(i, j)
        x === (i*10 + j + 11).toDouble
      }
      ok
    }
    
    "transpose" in {
      sut.transpose.row(0) === R2.Vector(11, 21)
      sut.transpose.row(1) === R2.Vector(12, 22)
      sut.transpose.row(2) === R2.Vector(13, 23)
    }
  }
}
