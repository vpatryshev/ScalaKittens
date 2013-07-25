package scalakittens

import org.specs.runner.JUnit4
import org.specs.Specification

/**
 * Unittests for FileSpec matcher
 */
class ContainsTest extends JUnit4(ContainsTest)

object ContainsTest extends Specification {
  trait Container[-T] {
    def contains(t:T): Boolean
  }

  trait MyList[+T] {
    def head:T
    def tail:MyList[T]
    def ::[U>:T](x:U) = new HeadWithTail(x, this)
    def size: Int
  }

  case object EmptyList extends MyList[Nothing] {
    def head = error("This list is empty")
    def tail = error("this list is empty")
    def size = 0
  }

  case class HeadWithTail[T](head:T, tail: MyList[T]) extends MyList[T] {
    def size = 1 + tail.size
  }

  implicit def asContainer[T](list:MyList[T]): Container[T] = new Container[T] {
    def contains(t:T) = list.size > 0 && (list.head == t || asContainer(list.tail).contains(t))
  }

  "My Code" should {

    "behave" in {
      case class A(name: String) {override def toString = "A(" + name + ")"}
      case class B(override val name: String) extends A(name) {override def toString = "B(" + name + ")"}

      val listB = B("1") :: B("2") :: EmptyList
      listB.size must_== 2
      val listA = A("3") :: listB
      listA.size must_== 3
      listA contains A("3") must beTrue
      listA contains B("1") must beTrue
//      listA contains "nothingness" is a compilation error
      val listC: MyList[Any] = listA
      listC contains "abracadabra" must beFalse
      listC contains B("2") must beTrue
    }
  }
}
