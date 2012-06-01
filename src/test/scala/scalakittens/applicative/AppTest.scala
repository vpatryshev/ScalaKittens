package scalakittens
package applicative

import org.specs._
import org.specs.runner.JUnit4
import scalakittens.applicative.All._
import scala.collection.immutable.List
import scala.collection.{Set, Seq}

class AppTest extends JUnit4(AppTest)

object AppTest extends Specification {

  val decorator = (s: String) => "***" + s + "***"

  val appender = (s1: String) => (s2: String) => s1 + " " + s2

  def cons[A](a: A)(as: List[A]) = a :: as

  "ApplicativeList" should {
    "combine two lists" in {
      import applicative.All.AppList._
      def pp(i: Int) = (j: Int) => i + j * j

      val combinations:List[Int] = ap(List(pp(1), pp(2)))(List(10, 20, 30))
      combinations == List(101, 401, 901, 102, 402, 902) mustBe true
    }

    "be able to zip two lists" in {
      import All.AppZip._

      val zp2: Seq[String => String => String] = pure(appender)
      (zp2 <*> List("1", "2") <*> List("a", "b")).toList == List("1 a", "2 b") mustBe true
    }

    "be able to zip functions with values" in {
      import All.AppZip._

      def prepend(prefix: String) = (s: String) => prefix + " " + s
      val result = (List(prepend("a"), prepend("the")) <*> List("quick brown fox jumps over ", "lazy dog "))
      result == List("a quick brown fox jumps over ", "the lazy dog ") aka result.toString mustBe true
    }

    "be able to transpose a matrix" in {
      import All.AppZip._

      trait matrices[A] {
        type matrix = Seq[Seq[A]]
        val glue: Seq[A => List[A] => List[A]] = pure(cons _)
        def transpose(m: matrix): matrix = if (m.isEmpty) Nil else transposeNonempty(m)
        def transposeNonempty(m: matrix): matrix = {
          m match {
            case Nil => pure(Nil)
            case row :: rows => {
              glue <*> row <*> transposeNonempty(rows)
            }
          }
        }
      }

      object m extends matrices[Int]
      import m._
      val m0x0: m.matrix = transpose(List[List[Int]]()).toList
      m0x0 == List() aka m0x0.toString mustBe true

      val m0x1: m.matrix = transpose(List(List[Int]()))
      m0x1 == List() aka m0x1.toString mustBe true

      val m2x1: m.matrix = transpose(List(List(1, 2)))
      m2x1 == List(List(1), List(2)) aka m2x1.toString mustBe true

      val m1x2: m.matrix = transpose(List(List(1), List(2))).toList
      m1x2 == List(List(1,2)) aka m1x2.toString mustBe true

      val m2x2 = transpose(List(List(11, 12), List(21, 22))).take(12).toList
      m2x2 equals List(List(11, 21), List(12, 22)) aka m2x2.toString mustBe true
    }
  }

  "ApplicativeSet" should {
    "combine Johns and Pauls" in {
      import All.AppSet._
      val combinations = pure(appender) <*> Set("John", "Paul") <*> Set("the 1st", "the 2nd")
      combinations == Set("John the 1st", "John the 2nd", "Paul the 1st", "Paul the 2nd") mustBe true
    }
  }
  
  "EnvApp" should {
    "evaluate an expression" in {

      def fetch(x: String) = (env: E) => env(x)

      val add = (i: Int) => (j: Int) => i + j

      trait Expr

      case class Var(x: String) extends Expr

      case class Val(i: Int) extends Expr

      case class Add(p: Expr, q: Expr) extends Expr

      def eval(exp: Expr): (E => Int) = exp match {
        case Var(x) => fetch(x)
        case Val(i) => KEnv(i)
        case Add(p, q) => KEnv(add) S eval(p) S eval(q)
      }

      object AppEnv extends AppEnv {
        override def f1[A, B](f: A => B): Env[A] => Env[B] = ea => f compose ea

        def ev(exp: Expr): (E => Int) = exp match {
          case Var(x) => fetch(x)
          case Val(i) => pure(i)
          case Add(p, q) => applicable[Int, Int => Int](pure(add)) <*> eval(p) <*> eval(q)
        }

      }
      val evaluated = AppEnv.ev(Add(Var("one"), Add(Var("three"), Val(11))))(Map("one" -> 1, "two" -> 2, "three" -> 3))
      evaluated must_== 15
    }
  }

  "Traversable" should {
    "do something, no?" in {
          import TraversableList._
          import AppSet._
      val thedist = dist(AppSet)
    }
  }
}
