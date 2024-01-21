package org.scalakittens.web.common.parsing.pdf

trait Chunk {
  val text: String
  val position: Int
  val width: Int
  lazy val center = position + width/2
  lazy val right = position + width/2 - 1
//  def containsPoint(x: Int) = math.abs(center - x) <= width/2
  def range = (position, width)
  def overlapsWith = SimpleTextDocument.intersect(range)

  lazy val toSource = s"""{"text":"$text"", "pos":$position, "width":$width}"""
}

object Chunk {
  def apply(t: String, p: Int, w: Int): Chunk = new Chunk {
    val text = t
    val position = p
    val width = w
  }
}