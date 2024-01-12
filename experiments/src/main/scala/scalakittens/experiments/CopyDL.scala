package scalakittens.experiments

import scala.language.postfixOps

object CopyDL extends App {

  /*
  The problem is this: we have a linked list of nodes; each node has a value and a pointer to another node anywhere 
  in the list.
  We have to make a copy of this list.
  Impossible without mutability or laziness. Haskell solution is laziness. Here's my solution.
   */

  trait Node {
    def value: Int

    def next: Node

    def random: Node
  }

  class DataNode(
    val value: Int,
    var next: Node,
    var random: Node) extends Node

  def inverseIndex(index: Array[Node]): Map[Node, Int] = {
    index.indices map { i => index(i) → i } toMap
  }

  def randIndex(index: Array[Node], inverseIndex: Map[Node, Int]): Array[Int] = {
    index map (_.random) map inverseIndex
  }

  def copy(nodes: Iterable[Node]): Iterable[Node] = {
    val index = nodes.toArray
    val ii = inverseIndex(index).withDefaultValue(-1)

    lazy val newNodes: List[IndirectNode] = index.indices map {
      i =>
        val old = index(i)
        IndirectNode(old.value, ii(old.next), ii(old.random))
    } toList

    case class IndirectNode(value: Int, nextId: Int, rndId: Int) extends Node {
      override def next: IndirectNode = if (nextId < 0) null else newNodes(nextId)

      override def random: IndirectNode = if (rndId < 0) null else newNodes(rndId)

      def valueOf(node: Node): String = Option(node) map (_.value.toString) orNull

      override def toString: String =
        s"Node($value, →${valueOf(next)}, →${valueOf(random)})"
    }

    newNodes
  }

  val n1 = new DataNode(101, null, null)
  val n2 = new DataNode(102, null, n1)
  val n3 = new DataNode(103, n1, n2)
  //n2.next = n3
  n1.next = n2
  n1.random = n3

  val copied = copy(n1 :: n2 :: n3 :: Nil)
  println(copied)

}
