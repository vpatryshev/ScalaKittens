package scalakittens.testing

import scalakittens.Functions.{predicate, predicate2}
import scalakittens.Library.RichPredicate2
import scalakittens.Result._
import scalakittens.{Function, Props, Result}
import org.specs2.execute.{Failure, FailureException}
import org.specs2.matcher._
import org.specs2.mutable.Specification

import scala.languageFeature.{implicitConversions, postfixOps}

/**
 * Use this class as a base for your tests.
 * It has a bunch of extra functionality.
 */
trait TestBase extends Specification {
  actual =>

//  implicit def np(u:Unit): MatchResult[Any] = ok
  def oops(what:Any): MatchResult[Boolean] = false aka what.toString must beTrue

  class StringChecker(s: String) {
    def mustContain(what: String): MatchResult[Any] = s contains what aka ("\"" + s + "\" should contain \"" + what + "\"") must_== true

    def mustNotContain(what: String): MatchResult[Any] = s contains what aka ("\"" + s + "\" should not contain \"" + what + "\"") must_== false

    override def toString: String = s
  }

  trait Expecting {
    def mustBeGood: MatchResult[Any]

    def mustBeGood(tag: String): MatchResult[Any]

    def mustBeBad: MatchResult[Any]
  }

  implicit def expectingGood(r: Result[_]): Expecting = new Expecting {
    def mustBeGood: MatchResult[Any] = if (!r.isGood) throw FailureException(Failure(s"Expected a good result, got $r")) else ok

    def mustBeGood(tag: String): MatchResult[Any] = if (!r.isGood) throw FailureException(Failure(s"$tag: Expected a good result, got $r")) else ok

    def mustBeBad: MatchResult[Any] = {
      r.isGood aka r.listErrors.mkString("; ") must_== false
    }
  }

  private implicit def akaMust2[T](tm: Expectable[T]): MustExpectable[T] = MustThrownExpectations.akaMust[T](tm) // jumping through implicit loops

  private implicit def desc[T](t: T): Descriptible[T] = describe(t)

  implicit def checkMe(s: String): StringChecker = new StringChecker(s)

  def doesNotContain(what: String)(s: String): MatchResult[Any] = s mustNotContain what

  implicit def functionValidator[X, Y](f: X => Y): predicate2[X, Y] =
    (x: X, y: Y) => f(x) == y

  class CheckThis[X, Y] {
    protected def check(checker: (X, Y) => MatchResult[Any], samples: (X, Y)*): MatchResult[Any] = {
      samples.forall(p => checker(p._1, p._2)) must beTrue
    }

    protected def check(checker: X => MatchResult[Any], samples: X*): MatchResult[Any] = {
      samples.forall(p => checker(p)) must beTrue
    }
  }

  class ValidatingPredicate[X](p: predicate[X]) extends CheckThis[X, Any] {
    def passesOn(samples: X*): MatchResult[Any] = {
      check(expectPositive(p), samples: _*)
    }

    def failsOn(samples: X*): MatchResult[Any] = {
      check(expectNegative(p), samples: _*)
    }

    private def expectPositive(op: predicate[X]): X => MatchResult[Any] = x => {
      op(x) aka ("False negative on \"" + x + "\"") must_== true
    }

    private def expectNegative(op: predicate[X]): X => MatchResult[Any] = x => {
      op(x) aka ("False positive on \"" + x + "\"") must_== false
    }

    def maps(samples: X*): Unit = passesOn(samples: _*)
  }

  implicit def validates[X](p2: predicate[X]): ValidatingPredicate[X] = new ValidatingPredicate[X](p2)

  class ValidatingPredicate2[X, Y](p2: predicate2[X, Y]) extends CheckThis[X, Y] {
    def passesOn(samples: (X, Y)*): MatchResult[Any] = {
      check(expectPositive(p2), samples: _*)
    }

    def failsOn(samples: (X, Y)*): Boolean = {
      check(expectNegative(p2), samples: _*)
    }

    private def expectPositive(op: predicate2[X, Y]): (X, Y) => MatchResult[Any] = (x, y) => {
      op(x, y) aka ("False negative on (\"" + x + "\", \"" + y + "\")") must_== true
    }

    private def expectNegative(op: predicate2[X, Y]): (X, Y) => MatchResult[Any] = (x, y) => {
      op(x, y) aka ("False positive on (\"" + x + "\", \"" + y + "\")") must_== false
    }

    def maps(samples: (X, Y)*): Unit = passesOn(samples: _*)
  }

  class RichCheckThis[X, Y] {
    protected def check(checker: (X, Y) => Outcome, samples: (X, Y)*): MatchResult[Any] = {

      val results: List[Outcome] = samples.toList map checker.tupled
      Result.traverse(results) mustBeGood
    }

    protected def check(checker: X => Outcome, samples: X*): MatchResult[Any] = {
      Result.traverse(samples.toList map checker) mustBeGood
    }
  }

  class ValidatingRichPredicate2[X, Y](p2: RichPredicate2[X, Y]) extends RichCheckThis[X, Y] {
    def passesOn(samples: (X, Y)*): MatchResult[Any] = {
      check(expectPositive(p2), samples: _*)
    }

    def failsOn(samples: (X, Y)*): Boolean = {
      check(expectNegative(p2), samples: _*)
    }

    private def expectPositive(op: RichPredicate2[X, Y]): (X, Y) => Outcome = (x, y) => {
      op(x, y) orCommentTheError "False negative on (\"" + x + "\", \"" + y     }

    private def expectNegative(op: RichPredicate2[X, Y]): (X, Y) => Outcome = (x, y) => {
      val res = Result.negate(op(x, y)) orCommentTheError "False positive on (\"" + x + "\", \"" + y + "\")"
      res
    }

    def maps(samples: (X, Y)*): Unit =  passesOn(samples: _*)
  }

  implicit def p2validates[X, Y](p2: predicate2[X, Y]): ValidatingPredicate2[X, Y] = new ValidatingPredicate2[X, Y](p2)

  implicit def p2validatesLoudly[X, Y](p2: RichPredicate2[X, Y]): ValidatingRichPredicate2[X, Y] = new ValidatingRichPredicate2[X, Y](p2)

  implicit def funValidates[X, Y](f: Function[X, Y]): ValidatingFunction[X, Y] = new ValidatingFunction[X, Y](f.name, f)

  class ValidatingFunction[X, Y](name: String, f: X => Y) extends CheckThis[X, Y] {

    def expectMatch(f: X => Y): (X, Y) => MatchResult[Any] = (x: X, y: Y) => {
      f(x) aka (name + "(" + x + ")=" + y) must_== y
    }

    def expectMa(f: X => Y): (X, Y) => MatchResult[Any] = (x: X, y: Y) => {
      try {
        val y1 = f(x)
        y1 aka (name + "(" + x + ")=" + y1) must_== y
      } catch {
        case mfe: MatchFailureException[_] => throw mfe
        case t: Throwable => throw new IllegalArgumentException(s"wtf, $t")
      }
    }

    def maps(samples: (X, Y)*): MatchResult[Any] = {
      check(expectMa(f), samples: _*)
    }
  }

  type Checker = (Props => MatchResult[Any]) => MatchResult[Any]

  def checkProps(expectedResults: String Map String)(props: Props): MatchResult[Any] = {
    val result:Outcome = BadIf(props.isEmpty) orCommentTheError s"Empty props, expected at least ${expectedResults.size} keys" andThen {
      val results = for (key <- expectedResults.keys) yield {
        val actual = props.get(key)
        val expected = expectedResults(key)
        actual match {
          case Some(x) => OKif(x == expected, s"'$key'->'$actual'")
          case None    => Result.error(s"Missing '$key'")
        }
      }
      Result.traverse(results) orCommentTheError props.toFormattedString
    }
    result onError ((e:Errors) => failure(s"Oops, ${e mkString "; "}"))
    ok
  }

  def checkProps(expectedResults: (String, String)*)(props: Props): MatchResult[Any] = checkProps(expectedResults.toMap)(props)
}
