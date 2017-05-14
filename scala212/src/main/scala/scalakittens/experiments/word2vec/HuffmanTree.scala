package scalakittens.experiments.word2vec

import scala.collection.mutable

/**
  * Huffman Tree implementation.
  * @see https://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html
  * Will need it for word2vec.
  * Due to the needs of hierarchical softmax in word2vec,
  * the array <code>parent</code> specifies whether this node is left or right;
  * for the left node <code>i</code>, <code>parent(i)</code> is positive; for the right node <code>i</code>, the value <code>parent(i)</code> is negative; its absolute value is the pointer to the parent.
  * For the root node, parent is 0.
  *
  * Created by vpatryshev on 5/2/17.
  */
class HuffmanTree(source: List[Int]) {
  val numWords: Int = source.length
  val size = numWords * 2 - 1
  private val parentRef = new Array[Int](size)

  private val frequencies = new mutable.Queue[Int]
  private val ordering: Ordering[(Int, Int)] = Ordering.by(-_._1)

  {
    val queue = new mutable.PriorityQueue[(Int, Int)]()(ordering)

    source.zipWithIndex foreach(p => {frequencies.enqueue(p._1); queue.enqueue(p)})

    while (queue.length > 1) {
      val p = queue.dequeue()
      val q = queue.dequeue()
      val node = (p._1+q._1, frequencies.length)
      queue.enqueue(node)
      parentRef(p._2) = frequencies.length
      parentRef(q._2) = -frequencies.length
      frequencies.enqueue(node._1)
    }
  }

  def parent(i: Int) = parentRef(i)
  
  lazy val freq = frequencies.toList


  /**
    * Chain of node indexes starting from word and going up to root
    * An element of the result list is positive if it is left branch, and negative if it is right
    *
    * @param word - id of the word, just an int
    * @return a list that represents the path from word to the root (inclusive)
    */
  def path(word: Int): List[Int] = {
    val idx = math.abs(word)
    val pr = parentRef(idx)
    word :: (if (pr == 0) Nil else path(pr))
  }

  override def toString: String = {
    s"HuffmanTree($frequencies, $parentRef)"
  }

  def toGraph: String = {
    case class RT/*rectangular text*/(content: List[String], w: Int, handle: Int)
    def merge(rt1: RT, rt2: RT): RT = {
      val gap = 2
      val w = rt1.w + gap + rt2.w
      val zip = rt1.content zip rt2.content
      val commonContent = zip map { case (s1, s2) => s1 + spaces(rt1.w + gap - s1.length) + s2}
      val newContent = commonContent ++
        rt1.content.drop(zip.length) ++
        rt2.content.drop(zip.length).map(s => spaces(rt1.w+gap) + s)
      RT(newContent, w, rt1.w+1)
    }

    def spaces(n: Int) = " "*n

    def dump(i: Int): RT = {
      val s = frequencies(i) + ":" + i

      if (i < numWords) {
        RT(s::Nil, s.length, s.length/2)
      } else {
        val i1 = parentRef.indexOf(i)
        val i2 = parentRef.indexOf(-i)
        val first = dump(i1)
        val second = dump(i2)
        val merged = merge(first, second)
        val pos = first.w - (s.length-1)/2
        val space = spaces(pos)
        val rightBranchPos = math.min(s.length/2 - 2, second.handle - 2)
        val spacesBetweenBranches = spaces(rightBranchPos)

        val branch1Length: Int = first.w - first.handle - 1
        val branch2Length: Int = second.handle - rightBranchPos/2 - 1

        val b1 = if (branch1Length > 2) "_"*(branch1Length-1) + "/" else "/"
        val b2 = if (branch2Length > 2) "\\" + "_"*(branch2Length-1) else "\\"
        val rbStart = first.handle+1 + b1.length + rightBranchPos + 2
        val b1b2 = spaces(first.handle+1) + b1 + spacesBetweenBranches + "  " + b2
        
        val branches = b1b2 :: (if (branch1Length > 2 || branch2Length > 2) {
          (spaces(first.handle) + "/" + spaces(b1.length + rightBranchPos+2 + math.max(b2.length, 2)) + "\\") :: Nil
        } else Nil)

        val header = (space + s) :: branches

        val res = RT(header ++ merged.content, merged.w, merged.handle)
        res
      }

    }
    dump(parentRef.length - 1).content mkString "\n"
  }
}

/* please ignore this comment
def p(tree: Node) {
  val pq = Queue[(Node, Int)]()
  pq.enqueue((tree, 0))

  var curLevel = 0 

  while (!pq.isEmpty) {
    val (node, level) = pq.dequeue
    if (level > curLevel) {
      println
      curLevel += 1
    }
    print(node.value)
    node.left.foreach(n => pq.enqueue((n, level+1)))
    node.right.foreach(n => pq.enqueue((n, level+1)))
  }
}
 */