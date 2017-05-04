package scalakittens.experiments.word2vec

import scala.collection.mutable

/**
  * Created by vpatryshev on 5/2/17.
  */
class HuffmanTree(source: List[Int]) {
  val parent = new Array[Int](source.length*2-1)
  parent(source.length*2-2) = -1
  val freq = new mutable.Queue[Int]
  private val ordering: Ordering[(Int, Int)] = Ordering.by(-_._1)
  
  private val queue = new mutable.PriorityQueue[(Int, Int)]()(ordering)

  source.zipWithIndex foreach(p => {freq.enqueue(p._1); queue.enqueue(p)})

  while (queue.length > 1) {
    val p = queue.dequeue()
    val q = queue.dequeue()
    val node = (p._1+q._1, freq.length)
    queue.enqueue(node)
    parent(p._2) = freq.length
    parent(q._2) = freq.length
    freq.enqueue(node._1)
  }
  
  override def toString: String = {
    s"HuffmanTree($freq, $parent)"
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
      RT(newContent, w, rt1.w + 1)
    }

    def spaces(n: Int) = " "*n
    
    def dump(i: Int): RT = {
      val s = freq(i) + ":" + i 
      if (parent.indexOf(i) < 0) {
        RT(s::Nil, s.length, s.length/2)
      } else {
        val i1 = parent.indexOf(i)
        val i2 = parent.indexOf(i, i1+1)
        val first = dump(i1)
        val second = dump(i2)
        val merged = merge(first, second)
        val pos = merged.handle
        val space = spaces(first.w)
        val rightBranchPos = math.min(s.length - 1, second.handle)
        val spacesBetweenBranches = spaces(rightBranchPos)

        val branch1Length: Int = first.w - first.handle - 1
        val branch2Length: Int = math.max(1, second.handle - 2)
        val branches = if (branch1Length > 2 || branch2Length > 2) {
          (spaces(first.handle + 1) + "-" * branch1Length + spaces(rightBranchPos+2) + "-" * branch2Length) :: Nil
        } else Nil
        
        val header = (space + s) :: (space + "/" + spaces(rightBranchPos) + "\\") :: branches
          
        RT(header ++ merged.content, merged.w, merged.handle)
      }

    }
    dump(parent.length - 1).content mkString "\n"
  }
  
  def chain(word: Int): List[(Int, Int)] = {
    
    (word, freq(word)) :: chain(parent(word))
  }
}
