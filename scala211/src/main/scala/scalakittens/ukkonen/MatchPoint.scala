package scalakittens.ukkonen

/**
  * When we match a sample, the result of matching will be
  * a pair of points, one is the position inside the sample,
  * starting with which the segment of sample matches 
  * a substring of the string to which we are matching,
  * the other one is the position within the string.
  * 
  * Created by vpatryshev on 2/6/17.
  */
case class MatchPoint(samplePos: Int, refPos: Int) {
  def location = refPos - samplePos

  override def equals(obj: scala.Any): Boolean = obj.isInstanceOf[MatchPoint] && obj.hashCode == hashCode

  override def hashCode(): Int = location
}
