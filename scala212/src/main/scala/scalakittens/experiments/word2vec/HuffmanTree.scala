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
  private val size: Int = source.length
  private val _parent = new Array[Int](source.length*2-1)

  _parent(source.length*2-2) = 0
  private val _freq = new mutable.Queue[Int]
  private val ordering: Ordering[(Int, Int)] = Ordering.by(-_._1)

  private val queue = new mutable.PriorityQueue[(Int, Int)]()(ordering)

  source.zipWithIndex foreach(p => {_freq.enqueue(p._1); queue.enqueue(p)})

  while (queue.length > 1) {
    val p = queue.dequeue()
    val q = queue.dequeue()
    val node = (p._1+q._1, _freq.length)
    queue.enqueue(node)
    _parent(p._2) = _freq.length
    _parent(q._2) = -_freq.length
    _freq.enqueue(node._1)
  }

  val parent = _parent.toList
  val freq = _freq.toList

  //  lazy val codes: Array[Int] = ??? // do we need it at all?

  override def toString: String = {
    s"HuffmanTree(${_freq}, ${_parent})"
  }

  def toGraph: String = {
    case class RT/*rectangular text*/(content: List[String], w: Int, handle: Int)
    def merge(rt1: RT, rt2: RT): RT = {
      val gap = 4
      val w = rt1.w + gap + rt2.w
      val zip = rt1.content zip rt2.content
      val commonContent = zip map { case (s1, s2) => s1 + spaces(rt1.w + 2 - s1.length) + s2}
      val newContent = commonContent ++
        rt1.content.drop(zip.length) ++
        rt2.content.drop(zip.length).map(s => spaces(rt1.w+2) + s)
      RT(newContent, w, rt1.w+1)
    }

    def spaces(n: Int) = " "*n

    def dump(i: Int): RT = {
      val s = _freq(i) + ":" + i

      if (i < size) {
        RT(s::Nil, s.length, s.length/2)
      } else {
        val i1 = _parent.indexOf(i)
        val i2 = _parent.indexOf(-i)
        val first = dump(i1)
        val second = dump(i2)
        val merged = merge(first, second)
        val pos = merged.handle
        val space = spaces(first.w)
        val rightBranchPos = math.min(s.length - 2, second.handle)
        val spacesBetweenBranches = spaces(rightBranchPos)

        val branch1Length: Int = first.w - first.handle - 1
        val branch2Length: Int = math.max(1, second.handle - rightBranchPos/2 - 1)
        val b1 = if (branch1Length == 1) "/" else "-"*branch1Length
        val b2 = if (branch2Length == 1) "\\" else "-"*branch2Length
        
        val branches = if (branch1Length > 2 || branch2Length > 2) {
          (spaces(first.handle+1) + b1 + spacesBetweenBranches + "  " + b2) ::
            (spaces(first.handle) + "/" + spaces(branch1Length + rightBranchPos+2 + branch2Length) + "\\") ::
            Nil
        } else Nil

        val header = (space + s) :: (space + "/" + spaces(rightBranchPos) + "\\") :: branches

        val res = RT(header ++ merged.content, merged.w, merged.handle)
        res
      }

    }
    dump(_parent.length - 1).content mkString "\n"
  }

  def chain(word: Int): List[(Int, Int)] = {

    (word, _freq(word)) :: chain(_parent(word))
  }
}
