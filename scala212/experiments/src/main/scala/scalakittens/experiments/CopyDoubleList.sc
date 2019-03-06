/*
The problem is this: we have a linked list of nodes; each node has a value and a pointer to another node anywhere in the list.
We have to make a copy of this list.
Impossible without mutability or laziness. Haskell solution is laziness. Here's my solution.
 */

trait Node {
  def value: Int
  def next: Node
  def random: Node
}

case class DataNode(value: Int, var next: Node, var random: Node) extends Node

def inverseIndex(index: Array[Node]): Map[Node, Int] = {
  index.indices map { i => index(i) -> i } toMap
}

def randIndex(index: Array[Node], inverseIndex: Map[Node, Int]): Array[Int] = {
  index map (_.random) map inverseIndex
}

def copy(nodes: Iterable[Node]): Iterable[Node] = {
  val index = nodes.toArray
  val ii = inverseIndex(index)
  
  lazy val newNodes: Array[IndirectNode] = index.indices map {
    i => 
      val old = index(i)
      IndirectNode(old.value, i+1, ii(old.random))
  } toArray
  
  case class IndirectNode(value: Int, nextId: Int, rndId: Int) extends Node {
    override def next = newNodes(nextId)
    override def random = newNodes(rndId)
  }

  newNodes
}

val n1 = DataNode(101, null, null)
val n2 = DataNode(102, null, n1)
val n3 = DataNode(103, n1, n2)
n2.next = n3; n1.next = n2; n1.random = n3

val copied = copy(n1::n2::n3::Nil)
println(copied)
