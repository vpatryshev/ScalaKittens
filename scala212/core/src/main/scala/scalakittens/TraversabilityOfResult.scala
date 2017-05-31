package scalakittens

import scala.collection._
import scala.reflect.ClassTag

/**
 * This trait contains tons of pesky details that any implementation of GenTraversableOnce should have
 * Unfortunately, Result's foreach returns itself, which contradicts the foreach contract in traversable. This should be fixed... or use tap instead
 * Created by vpatryshev on 10/18/15.
 */
trait TraversabilityOfResult[+T] extends GenTraversableOnce[T] with Container[T] {

  override def toStream: Stream[T] = toList.toStream

  override def seq: scala.TraversableOnce[T] = toList

  override def toSeq: GenSeq[T] = toList

  override def hasDefiniteSize: Boolean = true

  override def toArray[A1 >: T](implicit evidence$1: ClassTag[A1]): Array[A1] = toList.toArray

  override def toIterator: Iterator[T] = toList.iterator

  override def toVector: Vector[T] = toList.toVector

  override def toTraversable: GenTraversable[T] = toList

  override def isTraversableAgain: Boolean = true

  override def mkString(sep: String): String = mkString

//  override def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, T, Col[T]]): Col[T] = ???
}

trait EmptyTraversability[T] extends TraversabilityOfResult[T] with NothingInside[T] {
  I: NoGood[T] ⇒
  override def foldLeft[B](z: B)(op: (B, T) ⇒ B): B = z

  override def toMap[K, V](implicit ev: <:<[T, (K, V)]): GenMap[K, V] = Map.empty[K,V]

  private def cant(what: String) = throw new UnsupportedOperationException(s"Can't $what ${getClass.getSimpleName}")

  override def toList = Nil

  override def reduce[A1 >: T](op: (A1, A1) ⇒ A1): A1 = I cant "reduce"

  override def count(p: (T) ⇒ Boolean): Int = 0

  def aggregate[B](z: ⇒ B)(seqop: (B, T) ⇒ B,combop: (B, B) ⇒ B): B = z

  override def maxBy[B](f: (T) ⇒ B)(implicit cmp: Ordering[B]): T = I cant "maxBy"

  override def toSet[A1 >: T]: GenSet[A1] = Set.empty

  override def max[A1 >: T](implicit ord: Ordering[A1]): T = I cant "max"

  override def /:[B](z: B)(op: (B, T) ⇒ B): B = z

  override def product[A1 >: T](implicit num: Numeric[A1]): A1 = num.one

  override def size: Int = 0

  override def minBy[B](f: (T) ⇒ B)(implicit cmp: Ordering[B]): T = I cant "minBy"

  override def fold[A1 >: T](z: A1)(op: (A1, A1) ⇒ A1): A1 = z

  override def copyToArray[B >: T](xs: Array[B]): Unit = {}

  override def copyToArray[B >: T](xs: Array[B], start: Int): Unit = {}

  override def copyToArray[B >: T](xs: Array[B], start: Int, len: Int): Unit = {}

  override def reduceOption[A1 >: T](op: (A1, A1) ⇒ A1): Option[A1] = None

  override def min[A1 >: T](implicit ord: Ordering[A1]): T = I cant "min"

  override def forall(pred: (T) ⇒ Boolean): Boolean = true

  override def sum[A1 >: T](implicit num: Numeric[A1]): A1 = num.zero

  override def foldRight[B](z: B)(op: (T, B) ⇒ B): B = z

  override def reduceRightOption[B >: T](op: (T, B) ⇒ B): Option[B] = None

  override def reduceLeftOption[B >: T](op: (B, T) ⇒ B): Option[B] = None

  override def reduceRight[B >: T](op: (T, B) ⇒ B): B = I cant "reduceRight"

  override def mkString(start: String, sep: String, end: String): String = start+end

  override def mkString: String = ""

  override def find(pred: (T) ⇒ Boolean): Option[T] = None

  override def :\[B](z: B)(op: (T, B) ⇒ B): B = z
  
  override def exists(pred: (T) ⇒ Boolean): Boolean = false
}

trait NonemptyTraversability[T] extends TraversabilityOfResult[T] with SomethingInside[T] { me: Good[T] ⇒

  override def toList = {
    val list: List[T] = List(value)
    list
  }

  override def foldLeft[B](z: B)(op: (B, T) ⇒ B): B = op(z, value)

  override def toMap[K, V](implicit ev: <:<[T, (K, V)]): GenMap[K, V] = Map(value)

  override def reduce[A1 >: T](op: (A1, A1) ⇒ A1): A1 = value

  override def count(p: (T) ⇒ Boolean): Int = if (p(value)) 1 else 0

  def aggregate[B](z: B)(seqop: (B, T) ⇒ B, combop: (B, B) ⇒ B): B = seqop(z,value)

  override def maxBy[B](f: (T) ⇒ B)(implicit cmp: Ordering[B]): T = value

  override def toSet[A1 >: T]: GenSet[A1] = Set(value)

  override def max[A1 >: T](implicit ord: Ordering[A1]): T = value

  override def toSeq: GenSeq[T] = toList

  override def /:[B](z: B)(op: (B, T) ⇒ B): B = op(z, value)

  override def product[A1 >: T](implicit num: Numeric[A1]): A1 = value

  override def size: Int = 1

  override def minBy[B](f: (T) ⇒ B)(implicit cmp: Ordering[B]): T = value

  override def fold[A1 >: T](z: A1)(op: (A1, A1) ⇒ A1): A1 = value

  override def copyToArray[B >: T](xs: Array[B]): Unit = {xs(0) = value}

  override def copyToArray[B >: T](xs: Array[B], start: Int): Unit = {xs(start) = value}

  override def copyToArray[B >: T](xs: Array[B], start: Int, len: Int): Unit = {
    if (len > 0) xs(start) = value
  }

  override def reduceOption[A1 >: T](op: (A1, A1) ⇒ A1): Option[A1] = Option(value)

  override def min[A1 >: T](implicit ord: Ordering[A1]): T = value

  override def forall(pred: (T) ⇒ Boolean): Boolean = pred(value)

  override def sum[A1 >: T](implicit num: Numeric[A1]): A1 = value

  override def foldRight[B](z: B)(op: (T, B) ⇒ B): B = op(value, z)

  override def reduceRightOption[B >: T](op: (T, B) ⇒ B): Option[B] = Option(value)

  override def reduceLeftOption[B >: T](op: (B, T) ⇒ B): Option[B] = Option(value)

  override def reduceRight[B >: T](op: (T, B) ⇒ B): B = value

  override def mkString(start: String, sep: String, end: String): String = mkString

  override def mkString: String = value.toString

  override def find(pred: (T) ⇒ Boolean): Option[T] = toOption filter pred

  override def :\[B](z: B)(op: (T, B) ⇒ B): B = op(value, z)

  override def exists(pred: (T) ⇒ Boolean): Boolean = pred(value)
}