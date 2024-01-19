package scalakittens

import scala.collection._
import scala.reflect.ClassTag

/**
 * This trait contains tons of pesky details that any implementation of GenTraversableOnce should have
 * Unfortunately, Result's foreach returns itself, which contradicts the foreach contract in traversable. This should be fixed... or use tap instead
 * Created by vpatryshev on 10/18/15.
 */
trait Traversability[+T] extends IterableOnce[T] with Container[T] {

  def toStream: Stream[T] = this.toList.toStream

  def seq: IterableOnce[T] = this.toList

  def toSeq: Seq[T] = this.toList

  def hasDefiniteSize: Boolean = true

  def toArray[A1 >: T](implicit evidence$1: ClassTag[A1]): Array[A1] =
    this.toList.toArray

  def toIterator: Iterator[T] = this.toList.iterator

  def toVector: Vector[T] = this.toList.toVector

  def toTraversable: Iterable[T] = this.toList

  def isTraversableAgain: Boolean = true

//  override def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, T, Col[T]]): Col[T] = ???
}

trait EmptyTraversability[T] extends Traversability[T] with NothingInside[T] {
  I: NoGood[T] =>
  def foldLeft[B](z: B)(op: (B, T) => B): B = z

  def toMap[K, V](implicit ev: <:<[T, (K, V)]): Map[K, V] = Map.empty[K,V]

  private def cant(what: String) = throw new UnsupportedOperationException(s"Can't $what ${getClass.getSimpleName}")

  def toList = Nil

  def reduce[A1 >: T](op: (A1, A1) => A1): A1 = I cant "reduce"

  def count(p: (T) => Boolean): Int = 0

  def aggregate[B](z: => B)(seqop: (B, T) => B,combop: (B, B) => B): B = z

  def maxBy[B](f: (T) => B)(implicit cmp: Ordering[B]): T = I cant "maxBy"

  def toSet[A1 >: T]: GenSet[A1] = Set.empty

  def max[A1 >: T](implicit ord: Ordering[A1]): T = I cant "max"

  def /:[B](z: B)(op: (B, T) => B): B = z

  def product[A1 >: T](implicit num: Numeric[A1]): A1 = num.one

  def size: Int = 0

  def minBy[B](f: (T) => B)(implicit cmp: Ordering[B]): T = I cant "minBy"

  def fold[A1 >: T](z: A1)(op: (A1, A1) => A1): A1 = z

  def copyToArray[B >: T](xs: Array[B]): Unit = {}

  def copyToArray[B >: T](xs: Array[B], start: Int): Unit = {}

  def copyToArray[B >: T](xs: Array[B], start: Int, len: Int): Unit = {}

  def reduceOption[A1 >: T](op: (A1, A1) => A1): Option[A1] = None

  def min[A1 >: T](implicit ord: Ordering[A1]): T = I cant "min"

  def forall(pred: (T) => Boolean): Boolean = true

  def sum[A1 >: T](implicit num: Numeric[A1]): A1 = num.zero

  def foldRight[B](z: B)(op: (T, B) => B): B = z

  def reduceRightOption[B >: T](op: (T, B) => B): Option[B] = None

  def reduceLeftOption[B >: T](op: (B, T) => B): Option[B] = None

  def reduceRight[B >: T](op: (T, B) => B): B = I cant "reduceRight"

  def mkString(start: String, sep: String, end: String): String = start+end

  def mkString: String = ""

  def find(pred: (T) => Boolean): Option[T] = None

  def :\[B](z: B)(op: (T, B) => B): B = z

  def exists(pred: (T) => Boolean): Boolean = false
}

trait NonemptyTraversability[T] extends Traversability[T] with SomethingInside[T] { me: Good[T] =>

  def toList = {
    val list: List[T] = List(value)
    list
  }

  def foldLeft[B](z: B)(op: (B, T) => B): B = op(z, value)

  def toMap[K, V](implicit ev: <:<[T, (K, V)]): GenMap[K, V] = Map(value)

  def reduce[A1 >: T](op: (A1, A1) => A1): A1 = value

  def count(p: (T) => Boolean): Int = if (p(value)) 1 else 0

  def aggregate[B](z: B)(seqop: (B, T) => B, combop: (B, B) => B): B = seqop(z,value)

  def maxBy[B](f: (T) => B)(implicit cmp: Ordering[B]): T = value

  def toSet[A1 >: T]: Set[A1] = Set(value)

  def max[A1 >: T](implicit ord: Ordering[A1]): T = value

  override def toSeq: Seq[T] = toList

  def /:[B](z: B)(op: (B, T) => B): B = op(z, value)

  def product[A1 >: T](implicit num: Numeric[A1]): A1 = value

  def size: Int = 1

  def minBy[B](f: (T) => B)(implicit cmp: Ordering[B]): T = value

  def fold[A1 >: T](z: A1)(op: (A1, A1) => A1): A1 = value

  def copyToArray[B >: T](xs: Array[B]): Unit = {xs(0) = value}

  def copyToArray[B >: T](xs: Array[B], start: Int): Unit = {xs(start) = value}

  def copyToArray[B >: T](xs: Array[B], start: Int, len: Int): Unit = {
    if (len > 0) xs(start) = value
  }

  def reduceOption[A1 >: T](op: (A1, A1) => A1): Option[A1] = Option(value)

  def min[A1 >: T](implicit ord: Ordering[A1]): T = value

  def forall(pred: (T) => Boolean): Boolean = pred(value)

  def sum[A1 >: T](implicit num: Numeric[A1]): A1 = value

  def foldRight[B](z: B)(op: (T, B) => B): B = op(value, z)

  def reduceRightOption[B >: T](op: (T, B) => B): Option[B] = Option(value)

  def reduceLeftOption[B >: T](op: (B, T) => B): Option[B] = Option(value)

  def reduceRight[B >: T](op: (T, B) => B): B = value

  def mkString(start: String, sep: String, end: String): String = mkString

  def mkString: String = value.toString

  def find(pred: (T) => Boolean): Option[T] = asOption filter pred

  def :\[B](z: B)(op: (T, B) => B): B = op(value, z)

  def exists(pred: (T) => Boolean): Boolean = pred(value)
}