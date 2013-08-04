package scalakittens

import org.specs2.mutable.Specification

/**
 * example of natural transformation from covariant to contravariant functor
 */
class ContainsTest extends Specification {
  trait MyList[+T] {
    def head:T
    def tail:MyList[T]
    def ::[U>:T](x:U) = HeadAndTail(x, this)
    def size: Int
  }

  case object EmptyList extends MyList[Nothing] {
    def head = sys.error("This list is empty")
    def tail = sys.error("this list is empty")
    def size = 0
  }

  case class HeadAndTail[T](head:T, tail: MyList[T]) extends MyList[T] {
    def size = 1 + tail.size
  }

  trait Container[-T] {
    def contains(t:T): Boolean
  }

  implicit def asContainer[T](list:MyList[T]): Container[T] = new Container[T] {
    def contains(t:T) = list.size > 0 && (list.head == t || asContainer[T](list.tail).contains(t))
  }

  "My Code" should {

    "behave" in {
      class A(name: String) {
        override def toString = "A(" + name + ")"
      }
      class B(val name: String) extends A(name) {
        override def toString = "B(" + name + ")"}
      val b1= new B("1")
      val b2 = new B("2")
      val b3 = new B("3")
      val listB = b1 :: b2 :: EmptyList
      listB.size must_== 2
      val a3 = new A("3")
      val listA = a3 :: listB
      listA.size must_== 3
      listA contains a3 must beTrue
      listA contains b1 must beFalse
//      listA contains "nothingness" must not compile
//      listB contains a3 must not compile
      val cA: Container[A] = listA
      val cB: Container[B] = listB
      val cba: Container[B] = cA
      cA contains b3 must beTrue
      cba contains b3 must beFalse
      val listC: MyList[Any] = listA
      listC contains "abracadabra" must beFalse
      listC contains b2 must beTrue
//      type F[-A] = Function1[A, String]
//      val m1:F[Object] = Map("abc" -> "ABC")
//      val m2:F[String] = Map("xyz" -> "XYZ")
//      val m3:F[String] = m1
    }
  }
}
