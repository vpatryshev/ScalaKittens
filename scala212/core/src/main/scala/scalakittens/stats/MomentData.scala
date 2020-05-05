package scalakittens.stats

/**
  * Contains statistical data, provides aggregates
  * m0..m2 - moments of the data
  * Created by vpatryshev on 12/25/15.
  */
case class MomentData(m0: Int, m1: Double, m2: Double) {
  def add(x: Double) = MomentData(m0 + 1, m1 + x, m2 + x * x)

  def avg: Option[Double] = if (m0 < 1) None else Some(m1 / m0)

  def σ: Option[Double] = if (m0 < 2) None else Some((m2 - m1 * m1 / m0) / (m0 - 1))

  private def optDesc[T](v: Option[T], txt: String) = v map (txt+_) getOrElse ""
  private def dumpUs(strings: String*): String = strings filter(_.nonEmpty) mkString ", "

  override def toString = dumpUs(s"$m0 samples", optDesc(avg, "avg "), optDesc(σ, "σ "))
}

object MomentData {
  val Zero = MomentData(0, .0, .0)
}

