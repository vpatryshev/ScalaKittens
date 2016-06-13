package scalakittens

import language.implicitConversions
import org.specs2.matcher._
import Functions._
import StandardMatchResults._

trait MoreExpectations extends UsefulMocks { actual: Expectations ⇒

  type Checked = MatchResult[Any]
  type Checker[X] = X ⇒ Checked

  def aka(alt: Any):Expectable[Any] = actual.aka(alt.toString)

  implicit def wrapIt[T](x: T): MustExpectable[T] = MustExpectable(x)

  class StringChecker(s: String) {
    def mustContain   (what: String) = s contains what aka ("\"" + s + "\" should contain \""     + what + "\"") must_== true

    def mustNotContain(what: String) = s contains what aka ("\"" + s + "\" should not contain \"" + what + "\"") must_== false

    override def toString = s
  }

  implicit def expectingGood(r: Result[_]): Object {def mustBeGood: MatchResult[Any]} = new {
    def mustBeGood = r.isGood aka ("" + r.listErrors) must_== true
  }

  private implicit def akaMust2[T](tm: Expectable[T]): MustExpectable[T] = MustThrownExpectations.akaMust[T](tm) // jumping through implicit loops

  // private implicit def wrapIt[T](x: T): MustExpectable[T] = MustExpectable(x)
  /*
    implicit def expectingGood(r: Result[_]) = new {
      def mustBeGood = if (!r.isGood) throw new FailureException(new Failure("Expected a good result"))
    }
  */
  private implicit def desc[T](t:T) = describe(t)

  implicit def checkMe(s: String): StringChecker = new StringChecker(s)

  def doesNotContain(what: String)(s:String) = s mustNotContain what

  implicit def functionValidator[X, Y](f: X ⇒ Y): predicate2[X, Y] =
    (x: X, y: Y) ⇒ f(x) == y

  /*private[MoreExpectations]*/ class CheckThis[X, Y] {
    protected def check(checker: (X, Y) ⇒ _, samples: (X, Y)*) {
      samples.foreach(p ⇒ checker(p._1, p._2))
    }
    protected def check(checker: X ⇒ _, samples: X*) {
      samples.foreach(p ⇒ checker(p))
    }
    def ck(checker: (X, Y) ⇒ _, samples: (X, Y)*) = check(checker, samples:_*)
  }

  class ValidatingPredicate[X](p: predicate[X]) extends CheckThis[X, Any] {
    def passesOn(samples: X*) {
      check(expectPositive(p), samples:_*)
    }

    def failsOn(samples:X*) {
      check(expectNegative(p), samples:_*)
    }

    private def expectPositive(op: predicate[X]): Checker[X] = x ⇒ {
      op(x) aka ("False negative on \"" + x + "\"") must_== true
    }

    private def expectNegative(op: predicate[X]): Checker[X] = x ⇒ {
      op(x) aka ("False positive on \"" + x + "\"") must_== false
    }

    def maps(samples:X*) {
      passesOn(samples:_*)
    }
  }

  implicit def validates[X](p2: predicate[X]) = new ValidatingPredicate[X](p2)

  class ValidatingPredicate2[X, Y](p2: predicate2[X, Y]) extends CheckThis[X, Y] {
    def passesOn(samples:(X, Y)*) {
      check(expectPositive(p2), samples:_*)
    }

    def failsOn(samples:(X, Y)*) {
      check(expectNegative(p2), samples:_*)
    }

    private def expectPositive(op: predicate2[X, Y]): (X,Y) ⇒ Checked = (x, y) ⇒ {
      op(x, y) aka ("False negative on (\"" + x + "\", \"" + y + "\")") must_== true
    }

    private def expectNegative(op: predicate2[X, Y]): (X,Y) ⇒ Checked = (x, y) ⇒ {
      op(x, y) aka ("False positive on (\"" + x + "\", \"" + y + "\")") must_== false
    }

    def maps(samples:(X, Y)*) = {
      passesOn(samples:_*)
    }
  }

  implicit def p2validates[X, Y](p2: predicate2[X, Y]) = new ValidatingPredicate2[X, Y](p2)

  implicit def funValidates[X, Y](f: Function[X, Y]) = new ValidatingFunction[X, Y](f.name, f)

  class ValidatingFunction[X, Y](name: String, f: X ⇒ Y) extends CheckThis[X, Y] {

    def expectMatch[X,Y](f: X ⇒ Y) = (x: X, y: Y) ⇒ {
      try {
        val y1 = f(x)
        y1 aka (name + "(" + x + ")=" + y) must_== y
      } catch {
        case mfe: MatchFailureException[_] ⇒ throw mfe
        case t: Throwable ⇒ throw new IllegalArgumentException(s"wtf, $t")
      }
    }

    def maps(samples:(X, Y)*) {
      check(expectMatch(f), samples:_*)
    }
  }
}
