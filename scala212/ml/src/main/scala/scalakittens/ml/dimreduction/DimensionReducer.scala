package scalakittens.ml.dimreduction

import scalakittens.la.VectorSpace

/**
  * An abstraction for dimension reduction.
  * Just use it if you want to reduce something.
  * 
  * Created by vpatryshev on 6/11/17.
  */
trait DimensionReducer[S <: VectorSpace, T <: VectorSpace] {
  type SV = S#Vector
  type TV = T#Vector
  val source: S
  val target: T
  def reduce(vs: IndexedSeq[S#Vector]): IndexedSeq[T#Vector]
}
