package ukkonen.wip

/**
  * A leaf of a suffix tree. The leaf can consist of a chain of chars, and there's no continuation. So partial match means a failure.
  * Created by vpatryshev on 2/7/17.
  */
case class Leaf(suffixIndex: Int, start: Int, end: Int, tree: SuffixTreeData) extends GenNode {
  private val alignments = suffixIndex::Nil
  def listNodeAlignments: List[Int] = alignments

  object PartialMatch extends MatchResult {
    def alignments(str: Segment, idx: Int, howMany: Int) = SuffixTree.Empty
  }
  
  override def toString = s"Leaf($suffixIndex, $start, $end)" 
}
