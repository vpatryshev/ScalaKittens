package ukkonen.wip

import ukkonen.Lib
import Lib._

import scala.language.postfixOps

/**
  * A node of Ukkonen Suffix Tree
  * Created by vpatryshev on 2/6/17.
  */
class NodeInProgress(var start: Int, val end_ : Array[Int], val tree: SuffixTreeData) extends NodeBase { nip =>

  var suffixLink: NodeInProgress = _
  var suffixIndex: Int = -1
  def width = tree.width

  val childrenInProgress = new Array[NodeInProgress](tree.width)

  private[ukkonen] def childInProgress(b: Byte): NodeInProgress = tree.indexFor(b) map childrenInProgress orNull // TODO(vlad): get rid of nulls

  def goodKidsInProgress = childrenInProgress filterNot (null ==)

  def isLeaf = goodKidsInProgress.isEmpty

  private def setChild(i: Int)(node: NodeInProgress): Unit = {
    childrenInProgress(i) = node
  }

  def setChild(b: Byte): (NodeInProgress => Unit) = setChild(int(b))

  for {i <- 0 until tree.width} setChild(i)(null)

  def end = end_(0)

  def setSuffixIndex(index: Int): Unit = {
    val currentIndex = index - edgeLength
    goodKidsInProgress foreach (_.setSuffixIndex(currentIndex))
    if (isLeaf) suffixIndex = currentIndex
  }

  def children: (Int, Array[Point]) = {
    val ch0 = childrenInProgress.indexWhere(null !=)

    val ch1 = childrenInProgress.lastIndexWhere(null !=)
    val width = ch1 - ch0 + 1
    val cs = new Array[Point](width)
    for {
      i <- ch0 to ch1
    } cs(i-ch0) = Node.fromNip(childrenInProgress(i))
    (ch0, cs)
  }
  
  override def toString: String = s"Node($start, $end, '${tree.text(start)})"
}
