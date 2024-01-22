package scalakittens.ukkonen.wip

import scala.language.postfixOps
import scalakittens.ukkonen._
import wip.Lib._

/**
  * Created by vpatryshev on 2/6/17.
  */
trait Point {
  def listAlignmentsAt(str: Segment, idx: Int, howMany: Int): Set[MatchPoint]

  def listNodeAlignments: List[Int]
}

trait NodeBase {
  def start: Int

  def end: Int

  def edgeLength = end - start + 1

  def suffixIndex: Int

  val tree: SuffixTreeData
}

trait GenNode extends NodeBase with Point {

  private[ukkonen] trait MatchResult {
    def alignments(str: Segment, idx: Int, howMany: Int): Set[MatchPoint]
  }

  def listAlignmentsAt(str: Segment, idx: Int, howMany: Int): Set[MatchPoint] = {
    if (howMany <= 0) Set.empty[MatchPoint] else {
      val matched = traverseEdge(str, idx)
      matched.alignments(str, idx, howMany)
    }
  }

  def traverseEdge(seg: Segment, idx: Int): MatchResult = {
    val maxLength = Math.min(edgeLength, seg.length - idx)

    //Traverse the edge with character by character matching
    val edgeMatched =
      0 until maxLength forall (i => tree.text(start + i) == seg(idx + i))

    if (!edgeMatched) Mismatch
    else if (idx + maxLength >= seg.length) FullMatch
    else PartialMatch
  }

  object Mismatch extends MatchResult {
    def alignments(str: Segment, idx: Int, howMany: Int) = SuffixTree.Empty
  }

  object FullMatch extends MatchResult {
    def alignments(seg: Segment, idx: Int, howMany: Int) = {
      val listOfPoints = listNodeAlignments map (MatchPoint(seg.offset, _))
      listOfPoints.toSet take howMany
    }
  }

  def PartialMatch: MatchResult
}

abstract class Node(val children: Array[Point], ch0: Int, val tree: SuffixTreeData) extends GenNode {
  val width = children.length
  
  def indexFor(b: Byte) = Some(int(b) - ch0) filter (i => i >= 0 && i < width)

  val goodKids = children filterNot (null ==) toList

  private[ukkonen] def child(b: Byte): Point = {
    val maybePoint: Option[Point] = indexFor(b) map children filter (null !=)
    maybePoint getOrElse DeadEnd
  }

  lazy val listNodeAlignments: List[Int] = {
    goodKids flatMap (_.listNodeAlignments)
  }

  object PartialMatch extends MatchResult {
    def alignments(str: Segment, idx: Int, howMany: Int) = {
      val nextIdx = idx + edgeLength
      val c = str(nextIdx)
      val next = child(c)
      next.listAlignmentsAt(str, nextIdx, howMany)
    }
  }

}

class Root(children: Array[Point], ch0: Int, tree: SuffixTreeData) extends Node(children, ch0, tree) {
  val start = -1
  val end = -1
  override val edgeLength = 0

  def suffixIndex = throw new UnsupportedOperationException("Not implemented for root")

  override def traverseEdge(seg: Segment, idx: Int): MatchResult = PartialMatch
  
}

object Node {
  
  def fromNip(nip: NodeInProgress): Point = if (nip == null) null
  else {
    if (nip.goodKidsInProgress.isEmpty) {
      new Leaf(nip.suffixIndex, nip.start, nip.end, nip.tree)
    } else {
      val (ch0, cs) = nip.children

      new Node(cs, ch0, nip.tree) {
        val start = nip.start
        val end = if (nip.end_ == null) -1 else nip.end
        override val edgeLength = nip.edgeLength
        val suffixIndex = nip.suffixIndex

        override def toString = s"Node($start..$end)"
      }
    }
  }

  def fromRoot(sourceRoot: NodeInProgress): Node = {
    val (ch0, cs) = sourceRoot.children
    new Root(cs, ch0, sourceRoot.tree)
  }
}