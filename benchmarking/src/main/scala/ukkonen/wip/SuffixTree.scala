package ukkonen.wip

import ukkonen.{MatchPoint, Lib}
import Lib._

import scala.language.postfixOps

/**
  * Created by vpatryshev on 1/30/17.
  * Algorithm: https://www.cs.helsinki.fi/u/ukkonen/SuffixT1withFigs.pdf
  * src: http://www.geeksforgeeks.org/suffix-tree-application-2-searching-all-patterns/
  *
  * TODO(vlad): convert it into a good piece of Scala, currently it's C
  */
trait SuffixTreeData {
  val text: Array[Byte]
  
  lazy val width = dictionary.max + 1

  def int(b: Byte) = 0xff & b
  
  def indexFor(b: Byte) = Some(int(b)) filter (i => i < width)

  val dictionary: Array[Byte]

  def translate(source: Array[Byte]) = {
    val out = new Array[Byte](source.length)
    for { 
      i <- source.indices
    } out(i) = dictionary(source(i))
    
    out
  }
}

case class SuffixTreeBuilder(val source: Array[Byte]) extends SuffixTreeData {
  val DEBUG = false

  val dictionary: Array[Byte] = buildDictionary

  def buildDictionary: Array[Byte] = {
    val frequencies = new Array[(Int, Int)](256)
    for { i <- frequencies.indices} {
      frequencies(i) = (0, i)
    }
    for {b <- source} {
      frequencies(b) = (frequencies(b)._1 - 1, b)
    }
    val sorted = frequencies.sorted
    val out = new Array[Byte](256)
    for (i <- out.indices) out(i) = 255.toByte
    for {
      i <- sorted.indices 
      rec = sorted(i)
      if rec._1 < 0
    } out(sorted(i)._2) = i.toByte
    
    out
  }

  val text = translate(source)
  
  def println(x: => String) = if (DEBUG) System.out.println(x)

  private object root extends NodeInProgress(-1, null, this) { root =>

    override def edgeLength = 0

    override def toString: String = s"RootInProgress"
  }

  var lastNewNode: NodeInProgress = _
  var activeNode: NodeInProgress = root
  var activeEdge = -1
  var activeLength = 0

  var remainingSuffixCount = 0
  var leafEnd: Array[Int] = new Array[Int](1)
  leafEnd(0) = -1


  var numNodes = 0

  def newNode(start: Int, end: Int): NodeInProgress = {
    newNode(start, Array(end))
  }

  def newNode(start: Int, end_ : Array[Int]): NodeInProgress = {
    numNodes += 1
    val rt = Runtime.getRuntime
    val tm = (rt.totalMemory + M / 2) / M
    val fm = (rt.freeMemory + M / 2) / M
    val mm = (rt.maxMemory() + M / 2) / M

    try {
      val node = new NodeInProgress(start, end_, this)

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
        val am = tm - fm

        System.out.println(s"Got $oome, node#$numNodes, totalMem=$tm, free=$fm, allocated=$am, max=$mm")
        throw oome
    }
  }

  /*activePoint change for walk down (APCFWD) using
   Skip/Count Trick  (Trick 1). If activeLength is greater
   than current edge length, set next  internal node as
   activeNode and adjust activeEdge and activeLength
   accordingly to represent same activePoint*/
  def walkDown(currNode: NodeInProgress): Boolean = 
    if (activeLength >= currNode.edgeLength) {
      activeLength -= currNode.edgeLength
      activeEdge += currNode.edgeLength
      activeNode = currNode
      true
    } else false
  

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
      if (activeLength == 0) {
        activeEdge = pos //APCFALZ
      }

      // There is no outgoing edge starting with
      // activeEdge from activeNode
      if (activeEdge >= text.length) {
        throw new IndexOutOfBoundsException(s"We have activeEdge at $activeEdge, pos at $pos, text[${text.length}] = <<$text>>")
      }
      val ch: Byte = text(activeEdge)

      if (activeNode.childInProgress(ch) == null) {
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
        val next: NodeInProgress = activeNode.childInProgress(ch)
        if (walkDown(next)) {
          skipRestOfLoop = true
          //
        } else {
          skipRestOfLoop = false
          //Do walkdown or else Start from next node (the new activeNode)
          /*Extension Rule 3 (current character being processed
            is already on the edge)*/
          val nextStart: Int = next.start + activeLength
          if (text(nextStart) == text(pos)) {

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
            /*We will be here when activePoint is in middle of
             the edge being traversed and current character
             being processed is not  on the edge (we fall off
             the tree). In this case, we add a new internal node
             and a new leaf edge going out of that new node. This
             is Extension Rule 2, where a new leaf edge and a new
           internal node get created*/

            //New internal node
            val split: NodeInProgress = newNode(next.start, nextStart - 1)
            activeNode.setChild(ch)(split)

            //New leaf coming out of new internal node
            split.setChild(text(pos))(newNode(pos, leafEnd))
            next.start = nextStart
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
        if (activeNode != root) {
          activeNode = activeNode.suffixLink
        }
        else if (activeLength > 0) //APCFER2C1
        {
          //printf("\nGOING BACK!!! activeNode still root, al=%d", activeLength)
          activeLength -= 1
          activeEdge = pos - remainingSuffixCount + 1
        }
      }
    }
  }

  /*Build the suffix tree and print the edge labels along with
suffixIndex. suffixIndex for leaf edges will be >= 0 and
for non-leaf edges will be -1*/
  def build: SuffixTree = {
    val size = text.length

    for (i <- 0 until size) extendSuffixTree(i)

    root.setSuffixIndex(size)

    val roRoot = Node.fromRoot(root)
    
    SuffixTree(text, roRoot, dictionary)
  }
}

case class SuffixTree(val text: Array[Byte], val root: Node, val dictionary: Array[Byte]) extends SuffixTreeData {

  object Segment {
    def apply(src: Array[Byte]) = new Segment(translate(src), 0)

    def apply(src: String): Segment = apply(src.getBytes("UTF-8"))

  }
  
  def listAlignmentsOfSegment(segment: Segment, howMany: Int = Integer.MAX_VALUE): List[MatchPoint] = {
    val raw: Set[MatchPoint] = root.listAlignmentsAt(segment, 0, howMany)
    raw.toList.sortBy(p => p.location)
  }

  def listAlignments(bytes: Array[Byte], max: Int = Integer.MAX_VALUE): List[MatchPoint] =
    listAlignmentsOfSegment(Segment(bytes), max)

  def listAlignmentPoints(str: String, max: Int = Integer.MAX_VALUE): List[Int] =
    listAlignmentsOfSegment(Segment(str), max) map (_.location)

  def listSlidingAlignments(bytes: Array[Byte], len: Int, howMany: Int = Integer.MAX_VALUE): List[MatchPoint] = {
    if (len <= 0 || len > bytes.length)
      throw new scala.IllegalArgumentException(s"Bad length: $len (have ${bytes.length} bytes)")
    if (howMany <= 0) throw new scala.IllegalArgumentException(s"Bad limit: $howMany")

    val allOfThem: Set[MatchPoint] = allSlidingAlignmments(bytes, len, howMany)

    val sorted = allOfThem.toList.sortBy(_.location)
    sorted take howMany
  }

  def allSlidingAlignmments(bytes: Array[Byte], len: Int, howMany: Int): Set[MatchPoint] = {
    val translated = translate(bytes)

    val allOfThem: Set[MatchPoint] =
      (SuffixTree.Empty /: (0 to bytes.length - len)) {
        case (acc, offset) =>
          val segment: Segment = new Segment(translated, offset, len)
          acc ++ root.listAlignmentsAt(segment, 0, howMany - acc.size)
      }
    allOfThem
  }

  def contains(str: String): Boolean = listAlignmentPoints(str).nonEmpty
}


object SuffixTree {

  val Empty = Set.empty[MatchPoint]

  def fromBytes(source: Array[Byte]) =
    SuffixTreeBuilder(source).build

  def apply(source: String) =
    fromBytes(source.getBytes("UTF-8"))

}





