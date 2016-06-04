package scalakittens.scalaz
/*
import org.specs.runner.JUnit4
import org.specs.Specification
import java.io.{ByteArrayInputStream, PrintWriter, File}
import scalaz._
import ValidationPlus._

class ValidationTest extends JUnit4(ValidationTest)

object ValidationTest extends Specification {

  "ValidationPlus" should {
    "seamlessly and implicitly apply 'having'" in {
      val sut1: Validation[Int, String] = Success("success")
      val isGood: (String) => Boolean = _.startsWith("s")
      val sut2 = sut1.having(isGood)
      val sut3 = sut2 orElse 42
      sut3.isSuccess mustBe true
      sut3 must_== sut1
      val sut4 = sut1.having(isGood) orElse 42
      sut4.isSuccess mustBe true
      sut4 must_== sut1
      val sut5 = sut1.having(_.startsWith("k")) orElse 44
      sut5.isSuccess mustBe false
    }

    "deal properly with 'notNull'" in {
      notNull("xyz") orElse 123 must_== Success("xyz")
      notNull(java.util.Collections.EMPTY_MAP get "x") orElse 88 must_== Failure(88)
    }

    "convert Either to Validation" in {
      fromEither(Left("xyz")) must_== Failure("xyz")
      fromEither(Right(123)) must_== Success(123)
      val vr: Validation[Int, String] = Right("yesss!")
      vr must_== Success("yesss!")
      val vl: Validation[Int, String] = Left(9)
      vl must_== Failure(9)
    }

    "convert Option to Validation" in {
      val vr: Validation[Unit, String] = Some("yesss!")
      vr must_== Success("yesss!")
      val vl: Validation[Unit, String] = None
      vl must_== Failure(())
    }

  }
}
*/