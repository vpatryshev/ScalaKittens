package scalakittens.web

//import scala.language.postfixOps
import scalakittens.stats.Stats

/**
  * Created by vpatryshev on 8/8/15.
  * Keeps all kinds of stats and status variables
  */

object Vars {
    sealed class VarSet[T] {
        private val map = new scala.collection.mutable.HashMap[String, T]()
        def put(key: String, value: T): T = { map += (key → value); value }
        def apply(key: String): Option[T] = map.get(key)
        def show: String = {
            val sorted: List[(String, _)] = map.toList.sortWith((x,y) => x._1 < y._1)

            val strings: Iterable[String] = sorted.map(kv => s"${kv._1} → ${kv._2}")
            strings mkString "\n"
        }
    }

    val strings = new VarSet[String]()
    val stats = new VarSet[Stats]()

    private def newStats(name: String) = stats.put(name, new Stats)

    def addStats(name: String, value: Double = 1.0) = {
        stats(name) getOrElse newStats(name) add value
    }
}