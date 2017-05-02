package ukkonen

import scala.language.postfixOps
import Lib._

/**
  * Created by vpatryshev on 1/30/17.
  * Algorithm: https://www.cs.helsinki.fi/u/ukkonen/SuffixT1withFigs.pdf
  * src: http://www.geeksforgeeks.org/suffix-tree-application-2-searching-all-patterns/
  *
  * TODO(vlad): convert it into a good piece of Scala, currently it's C
  */
class SuffixTree(text: Array[Byte]) {
  def int(b: Byte) = 0xff & b
  
  val (ch0, ch1) = ((Integer.MAX_VALUE, 0) /: text) {
    case ((c0, c1), c) => (Math.min(c0, int(c)), Math.max(c1, int(c)))
  }
  lazy val maxChar = ch1 - ch0 + 1
  val DEBUG = false

  def println(x: => String) = if (DEBUG) System.out.println(x)

  //  def printf(x: String, args: Any*) = if (DEBUG) {
  //    System.out.printf(x, args:_*)
  //  }

  var nodecount: Int = 1

  case class Node(var start: Int, end_ : Array[Int]) {
    val id = nodecount
    nodecount += 1
    private val children0 = new Array[Node](maxChar)

    def child(i: Int): Node = children0(i)

    def child(c: Char): Node = {
      if (c < ch0) throw new IndexOutOfBoundsException(s"Oops, bad char $c, must be at least $ch0")
      child(c - ch0)
    }

    def isGood(b: Byte) = b >= ch0 && b <= ch1
    
    def child(b: Byte): Node = {
      if (!isGood(b)) throw new IndexOutOfBoundsException(s"Oops, bad char ${b.toChar}, must be between $ch0 and $ch1")

      child(b - ch0)
    }

    def setChild(i: Int)(node: Node): Unit = children0(i) = node

    def setChild(c: Char): (Node => Unit) = setChild(c - ch0)

    def setChild(b: Byte): (Node => Unit) = setChild(b - ch0)

    def goodKids = children0 filterNot (null ==)

    for {i <- 0 until maxChar} setChild(i)(null)
    var suffixLink: Node = _
    var suffixIndex: Int = -1

    def end = end_(0)

    override def toString: String = s"Node#$id($start, ${end_(0)}, '${if (start >= 0) text(start) else "?"})"
  }

  var root: Node = _

  var lastNewNode: Node = _
  var activeNode: Node = _
  var activeEdge = -1
  var activeLength = 0

  var remainingSuffixCount = 0
  var leafEnd: Array[Int] = new Array[Int](1)
  leafEnd(0) = -1
  var rootEnd = new Array[Int](1)
  var splitEnd = new Array[Int](1)
  var size = -1

  buildSuffixTree()

  var numNodes = 0

  def newNode(start: Int, end_ : Array[Int]): Node = {
    numNodes += 1
    val rt = Runtime.getRuntime
    val tm = (rt.totalMemory + M/2)/M
    val fm = (rt.freeMemory + M/2)/M
    val mm = (rt.maxMemory() + M/2)/M

    try {
      val node = new Node(start, end_)

      //    printf(s"\nJust created a new node, $node")

      /*For root node, suffixLink will be set to null
      For internal nodes, suffixLink will be set to root
      by default in  current extension and may change in
      next extension*/
      node.suffixLink = root

      /*suffixIndex will be set to -1 by default and
        actual suffix index will be set later for leaves
        at the end of all phases*/
      node
    } catch {
      case oome: OutOfMemoryError =>
        val am = tm-fm
        
        System.out.println(s"Got $oome, node#$numNodes, totalMem=$tm, free=$fm, allocated=$am, max=$mm")
        throw oome
    }
  }

  def edgeLength(n: Node) = {
    if (n == root) 0 else n.end - n.start + 1
  }

  def walkDown(currNode: Node): Boolean = {
    /*activePoint change for walk down (APCFWD) using
     Skip/Count Trick  (Trick 1). If activeLength is greater
     than current edge length, set next  internal node as
     activeNode and adjust activeEdge and activeLength
     accordingly to represent same activePoint*/
    if (activeLength >= edgeLength(currNode)) {
      activeEdge += edgeLength(currNode)
      activeLength -= edgeLength(currNode)
      activeNode = currNode
      true
    } else false
  }

  def extendSuffixTree(pos: Int): Unit = {
    /*Extension Rule 1, this takes care of extending all
    leaves created so far in tree*/
    leafEnd(0) = pos

    /*Increment remainingSuffixCount indicating that a
    new suffix added to the list of suffixes yet to be
    added in tree*/
    remainingSuffixCount += 1
    //printf("\nINcreasing rsc to %d at %d", remainingSuffixCount, pos)

    /*set lastNewNode to null while starting a new phase,
     indicating there is no internal node waiting for
     it's suffix link reset in current phase*/
    lastNewNode = null

    var broken: Boolean = false

    //Add all suffixes (yet to be added) one by one in tree
    while (!broken && remainingSuffixCount > 0) {
      var skipRestOfLoop = false
      //printf("\nrSC=%d, al=%d, pos=%d", remainingSuffixCount, activeLength, pos)
      if (activeLength == 0) {
        activeEdge = pos //APCFALZ
      }

      // There is no outgoing edge starting with
      // activeEdge from activeNode
      if (activeEdge >= text.length) {
        throw new IndexOutOfBoundsException(s"We have activeEdge at $activeEdge, pos at $pos, text[${text.length}] = <<$text>>")
      }
      val ch: Byte = text(activeEdge)

      if (activeNode.child(ch) == null) {
        val newEnd = new Array[Int](1)
        newEnd(0) = leafEnd(0)
        //        printf("\nAdding child %c -> %d - %d", ch, pos, newEnd(0))
        //Extension Rule 2 (A new leaf edge gets created)
        activeNode.setChild(ch)(newNode(pos, leafEnd))

        /*A new leaf edge is created in above line starting
         from  an existng node (the current activeNode), and
         if there is any internal node waiting for it's suffix
         link get reset, point the suffix link from that last
         internal node to current activeNode. Then set lastNewNode
         to null indicating no more node waiting for suffix link
         reset.*/
        if (lastNewNode != null) {
          lastNewNode.suffixLink = activeNode
          lastNewNode = null
        }
      } else {
        // There is an outgoing edge starting with activeEdge
        // from activeNode
        // Get the next node at the end of edge starting
        // with activeEdge
        val next: Node = activeNode.child(ch)
        if (walkDown(next)) {
          skipRestOfLoop = true
          //
        } else {
          skipRestOfLoop = false
          //Do walkdown or else Start from next node (the new activeNode)
          //          printf("\n%d vs %d", next.start + activeLength, pos)
          /*Extension Rule 3 (current character being processed
            is already on the edge)*/
          if (text(next.start + activeLength) == text(pos)) {

            //If a newly created node waiting for it's 
            //suffix link to be set, then set suffix link 
            //of that waiting node to curent active node
            if (lastNewNode != null && activeNode != root) {
              lastNewNode.suffixLink = activeNode
              lastNewNode = null
            }

            //APCFER3
            activeLength += 1
            /*STOP all further processing in this phase
            and move on to next phase*/
            broken = true
          }

          if (!broken) {
            /*We will be here when activePoint is in middle of\]
             the edge being traversed and current character
             being processed is not  on the edge (we fall off
             the tree). In this case, we add a new internal node
             and a new leaf edge going out of that new node. This
             is Extension Rule 2, where a new leaf edge and a new
           internal node get created*/
            splitEnd = new Array[Int](1)
            splitEnd(0) = next.start + activeLength - 1

            //            printf(s"\nnew split node ${next.start} - ${splitEnd(0)}, at $splitEnd")
            //New internal node
            val split: Node = newNode(next.start, splitEnd)
            activeNode.setChild(ch)(split)

            //New leaf coming out of new internal node
            split.setChild(text(pos))(newNode(pos, leafEnd))
            next.start += activeLength
            split.setChild(text(next.start))(next)

            /*We got a new internal node here. If there is any
              internal node created in last extensions of same
              phase which is still waiting for it's suffix link
              reset, do it now.*/
            if (lastNewNode != null) {
              /*suffixLink of lastNewNode points to current newly
                created internal node*/
              lastNewNode.suffixLink = split
            }

            /*Make the current newly created internal node waiting
              for it's suffix link reset (which is pointing to root
              at present). If we come across any other internal node
              (existing or newly created) in next extension of same
              phase, when a new leaf edge gets added (i.e. when
              Extension Rule 2 applies is any of the next extension
              of same phase) at that point, suffixLink of this node
              will point to that internal node.*/
            lastNewNode = split
          }

        }
      }

      if (!broken && !skipRestOfLoop) {
        /* One suffix got added in tree, decrement the count of
          suffixes yet to be added.*/
        remainingSuffixCount -= 1
        //printf("\nDecreasing rsc to %d", remainingSuffixCount)
        if (activeNode == root && activeLength > 0) //APCFER2C1
        {
          //printf("\nGOING BACK!!! activeNode still root, al=%d", activeLength)
          activeLength -= 1
          activeEdge = pos - remainingSuffixCount + 1
        }
        else if (activeNode != root) //APCFER2C2
        {
          activeNode = activeNode.suffixLink
        }
      }
    }
  }

  def dumpSegment(i: Int, j: Int): Unit = {
    for (k <- i to j) print(text(k))
  }

  //Print the suffix tree as well along with setting suffix index
  //So tree will be printed in DFS manner
  //Each edge along with it's suffix index will be printed
  def setSuffixIndexByDFS(n: Node, labelHeight: Int): Unit = {
    if (n == null) return

    if (n.start != -1) //A non-root node
    {
      //Print the label on edge from parent to current node
      //Uncomment below line to print suffix tree
      //printf("setSufindex: %d - %d", n.start, n.end)
    }
    var leaf = 1
    for (i <- 0 until maxChar) {
      val nextNode: Node = n.child(i)
      if (nextNode != null) {
        //Uncomment below two lines to print suffix index
        if (leaf == 1 && n.start != -1)
          println(s" [${n.suffixIndex}]")

        //Current node is not a leaf as it has outgoing
        //edges from it.
        println(s"Next node = $nextNode")
        leaf = 0
        setSuffixIndexByDFS(nextNode, labelHeight +
          edgeLength(nextNode))
      }
    }
    if (leaf == 1) {
      n.suffixIndex = size - labelHeight
      if (n.suffixIndex < 0) throw new IndexOutOfBoundsException(s"Size=$size, lh=$labelHeight")
    }
  }

  /*Build the suffix tree and print the edge labels along with
suffixIndex. suffixIndex for leaf edges will be >= 0 and
for non-leaf edges will be -1*/
  def buildSuffixTree(): Unit = {
    size = text.length

    rootEnd = new Array[Int](1)
    rootEnd(0) = -1

    /*Root is a special node with start and end indices as -1,
    as it has no parent from where an edge comes to root*/
    root = newNode(-1, rootEnd)

    activeNode = root //First activeNode will be root
    for (i <- 0 until size) extendSuffixTree(i)

    setSuffixIndexByDFS(root, labelHeight = 0)
    //    printf(s"\nroot: $root")
    //    for (c <- root.children if c != null) {
    //      printf(s"\n  child $c")
    //    }
    //    println("---")
  }

  def traverseEdge(str: Segment, idx0: Int, start: Int, end: Int): Int = {
    var k = start
    var idx = idx0
    //Traverse the edge with character by character matching
    while (k <= end && idx < str.length) {
      if (text(k) != str(idx)) return -1 // mo match

      k += 1
      idx += 1
    }

    if (idx >= str.length) 1 else 0 // match or more characters yet to match
  }

  private def listNodeAlignments(n: Node): List[Int] = {
    val result = if (n == null) Nil
    else if (n.suffixIndex > -1) {
      println(s"\nFound at position: ${n.suffixIndex}")
      n.suffixIndex :: Nil
    } else {
      val goodKids: Array[Node] = n.goodKids
      goodKids.toList flatMap listNodeAlignments
    }
    println(s"WHAT FOUND: $result")
    result
  }

  val Empty = Set.empty[MatchPoint]

  def isGood(b: Byte) = b >= ch0 && b <= ch1

  private def listAlignmentsAt(n: Node, str: Segment, idx0: Int, max: Int = Integer.MAX_VALUE): Set[MatchPoint] = {
    var idx = idx0
    
    if (n == null) return Empty // no match
    //    println(s"doTraversal(${n.start}-${n.end}, $str, $idx)")

    var res = -1
    //If node n is not root node, then traverse edge
    //from node n's parent to node n.
    if (n.start != -1) {
      res = traverseEdge(str, idx, n.start, n.end)
      //      println(s"Traversed with $idx, got $res")
      if (res == -1) //no match
        return Empty
      if (res == 1) //match
      {
        val found: Set[MatchPoint] = listNodeAlignments(n) map (MatchPoint(str.offset, _)) toSet
        
        return found take max
      }
    }
    //Get the character index to search
    idx += edgeLength(n)
    val c = str(idx)

    //If there is an edge from node n going out
    //with current character str[idx], travrse that edge
    if (!isGood(c)) Empty else listAlignmentsAt(n.child(c), str, idx, max)
  }

  def listAlignmentsOfSegment(segment: Segment, max: Int): List[MatchPoint] = {
    val raw: Set[MatchPoint] = listAlignmentsAt(root, segment, 0, max)
    val sorted = raw.toList.sortBy(p => p.location)
    sorted
  }

  def listAlignmentPoints(str: String, max: Int = Integer.MAX_VALUE): List[Int] = listAlignmentsOfSegment(Segment(str), max) map (_.location)

  def listAlignments(bytes: Array[Byte], max: Int = Integer.MAX_VALUE): List[MatchPoint] = listAlignmentsOfSegment(Segment(bytes), max)

  def listSlidingAlignments(bytes: Array[Byte], len: Int, max: Int): List[MatchPoint] = {
    if (len <= 0 || len > bytes.length) 
                  throw new IllegalArgumentException(s"Bad length: $len (have ${bytes.length} bytes)")
    if (max <= 0) throw new IllegalArgumentException(s"Bad limit: $max")
    
    val allOfThem:Set[MatchPoint] = 
      (Empty /: (0 to bytes.length-len)) {
        case (acc, offset) => acc ++ listAlignmentsAt(root, Segment(bytes, offset, len), 0, max - acc.size)
      }
    
    val sorted = allOfThem.toList.sortBy(_.location)
    sorted take max
  }
  
  def checkForSubstring(str: String): Boolean = {
    listAlignmentPoints(str).nonEmpty
  }

}

object SuffixTree {
  val M = 1<<20

  def fromBytes(source: Array[Byte]) =
    new SuffixTree(source)

  def apply(source: String) =
    fromBytes(source.getBytes("UTF-8"))

}

case class Segment(data: Array[Byte], offset: Int = 0, length0: Int = Integer.MAX_VALUE) {
  val length = Math.min(data.length - offset, length0)
  def apply(idx: Int) = {
    val i: Int = offset + idx
    if (i >= data.length) throw new ArrayIndexOutOfBoundsException(s"size=${data.length}, length=$length, offset=$offset, idx=$idx, oops...")
    data(i)
  }
}

object Segment {
  def apply(src: Array[Byte]) = new Segment(src, 0)

  def apply(src: String): Segment = apply(src.getBytes("UTF-8"))

}
