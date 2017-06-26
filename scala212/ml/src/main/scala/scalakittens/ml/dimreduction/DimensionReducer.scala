package scalakittens.ml.dimreduction

/**
  * An abstraction for dimension reduction.
  * Just use it if you want to reduce something.
  * 
  * Created by vpatryshev on 6/11/17.
  */
trait DimensionReducer[SV, TV] {
  def reduce(vs: IndexedSeq[SV]): IndexedSeq[TV]
}
