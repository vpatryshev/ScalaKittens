package scalakittens.ml.dimreduction
import scalakittens.la.Vector

/**
  * Created by vpatryshev on 6/11/17.
  */
trait DimensionReductor extends (IndexedSeq[Vector] => IndexedSeq[Vector]) {
  val sourceDim: Int
  val targetDim: Int
}
