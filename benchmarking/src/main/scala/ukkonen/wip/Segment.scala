package ukkonen.wip

import java.util

/**
  * Segment of text for matching
  * Created by vpatryshev on 2/6/17.
  */
class Segment(val data: Array[Byte], val offset: Int = 0, val maxLength: Int = Integer.MAX_VALUE) {
  val length = Math.min(data.length - offset, maxLength)

  def apply(idx: Int) = {
    val i: Int = offset + idx
    if (i >= data.length) throw new ArrayIndexOutOfBoundsException(s"size=${data.length}, length=$length, offset=$offset, idx=$idx, oops...")
    data(i)
  }
  override def toString: String = s"Segment(${util.Arrays.toString(data)}, offset, maxLength)}"
}
