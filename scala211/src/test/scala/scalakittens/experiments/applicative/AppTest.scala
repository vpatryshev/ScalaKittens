package scalakittens.experiments.applicative

import org.specs2.mutable.Specification

import scala.collection.immutable.List
import scala.collection.{Seq, Set}
import scalakittens.experiments.algebra.{Semigroup, Monoid}
import scalakittens.experiments.applicative.All._

object AppTest extends Specification {

  val decorator = (s: String) ⇒ "***" + s + "***"

  val appender = (s1: String) ⇒ (s2: String) ⇒ s1 + " " + s2

  def cons[A](a: A)(as: List[A]) = a :: as

  "ApplicativeList" should {
    "combine two lists" in {
      def pp(i: Int) = (j: Int) ⇒ i + j * j
      import AppList._
      val combinations:List[Int] = ap(List(pp(1), pp(2)))(List(10, 20, 30))
      combinations must_== List(101, 401, 901, 102, 402, 902)
    }
  }

  "ZipList" should {
    import All.AppZip._
    "be able to zip two lists" in {
      val appender = (s1: String) ⇒ (s2: String) ⇒ s1 + " " + s2

      val zp2: Seq[String ⇒ String ⇒ String] = pure(appender)
      (zp2 <*> List("1", "2") <*> List("a", "b")).toList must_== List("1 a", "2 b")
    }

    "be able to zip functions with values" in {

      def prepend(prefix: String) = (s: String) ⇒ prefix + " " + s
      val result = List(prepend("a"), prepend("the")) <*> List("quick brown fox jumps over ", "lazy dog ")
      result must_== List("a quick brown fox jumps over ", "the lazy dog ")
    }

    "be able to transpose a matrix" in {

      trait matrices[A] {
        type matrix = Seq[Seq[A]]
        val glue: Seq[A ⇒ List[A] ⇒ List[A]] = pure(cons _)
        def transpose(m: matrix): matrix = if (m.isEmpty) Nil else transposeNonempty(m)
        def transposeNonempty(m: matrix): matrix = {
          m match {
            case Nil ⇒ pure(Nil)
            case row :: rows ⇒ glue <*> row <*> transposeNonempty(rows)
          }
        }
      }

      object m extends matrices[Int]
      import m._
      val m0x0: m.matrix = transpose(List[List[Int]]()).toList
      m0x0 must_== List()

      val m0x1: m.matrix = transpose(List(List[Int]()))
      m0x1 must_== List()

      val m2x1: m.matrix = transpose(List(List(1, 2)))
      m2x1 must_== List(List(1), List(2))

      val m1x2: m.matrix = transpose(List(List(1), List(2))).toList
      m1x2 must_== List(List(1,2))

      val m2x2 = transpose(List(List(11, 12), List(21, 22))).take(12).toList
      m2x2 must_== List(List(11, 21), List(12, 22))
    }
  }

  "ApplicativeSet" should {
    "combine Johns and Pauls" in {
      import All.AppSet._
      val combinations = pure(appender) <*> Set("John", "Paul") <*> Set("the 1st", "the 2nd")
      combinations == Set("John the 1st", "John the 2nd", "Paul the 1st", "Paul the 2nd") must beTrue
    }
  }
  
  "AppEnv" should {
    "evaluate an expression" in {
      import EnvExample._

      object Expressions extends AppEnv {

        val fetch = (varName: String) ⇒ (env: Env) ⇒ env(varName)
        val const = (value: Int) ⇒ (env: Env) ⇒ value
        val add = (i: Int) ⇒ (j: Int) ⇒ i + j

        trait Expr {
          def eval: Env ⇒ Int
        }

        case class Var(name: String) extends Expr {
          override def eval = fetch(name)
        }

        case class Val(i: Int) extends Expr {
          override def eval = const(i)
        }

        case class Add(p: Expr, q: Expr) extends Expr {
          override def eval =  pure(add) <*> p.eval <*> q.eval
        }


      }

      import Expressions._

      val expr = Add(Var("one"), Add(Var("three"), Val(11))).eval
      expr (Map("one" -> 1, "two" -> 2, "three" -> 3)) must_== 15
    }
  }

  "Traversable" should {
    "let applicative set traverse over list" in {
      import TraversableList._

      val distributor = dist[String, Set](AppSet)
      val setOfLists = distributor(List(Set("a", "b"), Set("x", "y")))
      setOfLists must_== Set(List("a", "x"), List("a", "y"), List("b", "x"), List("b", "y"))
    }

    "let applicative list traverse over tree" in {
      import TraversableTree._

      val distributor = dist[String, List](AppList)
      val treeOfLists:Tree[List[String]] = Node(Leaf(List("a", "b")), Node(Leaf(List("x", "y", "z")), Leaf(List("1"))))
      val listOfTrees = distributor(treeOfLists)
      println(listOfTrees)
      def tree(s: String) = Node(Leaf("" + s.charAt(0)), Node(Leaf("" + s.charAt(1)), Leaf("" + s.charAt(2))))
      listOfTrees must_== List(tree("ax1"), tree("ay1"), tree("az1"), tree("bx1"), tree("by1"), tree("bz1"))
    }
  }
  
  "Monoid" should {
    "walk through the tree collecting the content" in {
      object StringMonoid extends Monoid[String] {
        val _0 = ""
        def add(x: String, y: String) = x + y
      }
      import TraversableTree._
      def tree(s: String) = Node(Leaf(s.substring(0,1)), Node(Leaf(s.substring(1,2)), Leaf(s.substring(2,3))))
      val sut: Tree[String] = tree("abc")
      val collected: String = traverse(StringMonoid.App)((s: String) ⇒ "<<" + s + ">>")(sut).value
      collected must_== "<<a>><<b>><<c>>"
    }
  }

  trait ListMonoid[T] extends Monoid[List[T]] {
    val _0 = Nil
    def add(x: List[T], y: List[T]) = x ++ y
  }

  object ExceptionLog extends ListMonoid[Exception]

  "AccumulativeErrors" should {
    "work properly" in {
      import All.AccumulatingErrors
      trait Oops
      case class Omg(what: String) extends Oops
      case class OiVei(first: Oops, second: Oops) extends Oops

      object Accumulator extends AccumulatingErrors[Oops] {
        val errorLog =  new Semigroup[Oops] { def add(x: Oops, y: Oops) = OiVei(x, y) }
      }

      implicit def applicable[A, B](maybeF: Either[Oops, A ⇒ B]): Applicable[A, B, Accumulator.Maybe] = Accumulator.App.applicable(maybeF)

      val goodFun    = Right((s: String) ⇒ "<<" + s + ">>")
      val badFun     = Left(Omg("snafu"))
      val goodString = Right("I am good")
      val badString  = Left(Omg("I'm bad"))
      goodFun <*> goodString must_== Right("<<I am good>>")
      goodFun <*> badString  must_== Left(Omg("I'm bad"))
      badFun  <*> goodString must_== Left(Omg("snafu"))
      badFun  <*> badString  must_== Left(OiVei(Omg("snafu"), Omg("I'm bad")))
    }
  }
}
