/**
 * A bunch of common Html tricks
 * Created by vpatryshev on 2/11/14.
 */
object Html {

  def textToRefPair(node:Node):Option[(String, String)] = {
    attribute("href") of node map (_ -> node.text)
  }

  def isLocalRef(k: String) = k != "#" && (k startsWith "#")

  // Note. This is a little bit weird: we map anchors to texts; there may be more than one such text,
  // we take the last one. The problem is, we use texts as prefixes, and we take only one prefix
  // of course this is non-deterministic. But we will see.
  def anchors(html:NodeSeq): String Map String = {
    val references = html \\ "a" flatMap textToRefPair
    val localReferences = references filter (r ⇒ isLocalRef(r._1))
    localReferences map {case (k,v) ⇒ (k.toString.tail, v)} toMap
  }

  def attribute(name:String) = new {
    def of(node:Node) = node.attribute(name).map(_.mkString)
  }

}
