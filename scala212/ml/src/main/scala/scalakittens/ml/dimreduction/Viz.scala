package scalakittens.ml.dimreduction

/**
  * Created by vpatryshev on 7/10/17.
  */
object Viz {
  val N = 120 /*120*/
  val M = 30 /*60*/
  def visualize(title: String, projections: Seq[(Any, Double, Double)]): Unit = {
    println
    println
    println("=" * N)
    println(s"                                    $title\n")
    println("-" * N)
    val xs = projections.map(_._2)
    val ys = projections.map(_._3)

    val (xmin, xmax) = (xs.min, xs.max)
    val (ymin, ymax) = (ys.min, ys.max)

    val xScale = (xmax - xmin) / N
    val yScale = (ymax - ymin) / M

    val sample = projections map {
      case (w, x, y) => (w, ((x - xmin) / xScale).toInt, ((y - ymin) / yScale).toInt)
    }

    val samplesByLine = sample.groupBy(_._3)

    0 to M foreach {
      j =>
        val row = samplesByLine.getOrElse(j, Nil) map (t => t._1 -> t._2)

        val layout = (Map[Int, Char]() /: row) {
          case (charMap, (z, pos)) =>
            val w = z.toString
            val wordRange = math.max(pos - 1, 0) until math.min(N, pos + w.length + 1)
            if (wordRange exists charMap.contains) charMap else {
              val m1 = 0 until w.length map (i => i + pos -> w.charAt(i)) toMap

              charMap ++ m1
            }
        }

        val chars = 0 to N map (layout.getOrElse(_, ' '))

        print(chars mkString)
        println
    }
  }
}
