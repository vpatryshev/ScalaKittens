package ukkonen.wip

/**
  * A special kind of node that not even a node, and replaces all those nulls in an array
  * Created by vpatryshev on 2/7/17.
  */
object DeadEnd extends Point {
  override def listNodeAlignments = Nil
  def listAlignmentsAt(str: Segment, idx: Int, howMany: Int) = SuffixTree.Empty
  
  override def toString = "DeadEnd"
}
