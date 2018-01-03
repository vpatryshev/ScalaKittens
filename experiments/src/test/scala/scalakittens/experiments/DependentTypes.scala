package scalakittens.experiments

/**
  * Demonstrating how one can maniputate types that strictly depend on 
  * instances of other classes
  */
class DependentTypes {
  import language.reflectiveCalls
  /**
    * Enclosing class the instances of which are used to identify types
    * of member instances
    * @param name for debugging
    */
  class Board(name: String) { c =>
    override def toString = s"C$name"

    /**
      * Inner class, the type of which can be treated as dependent on the 
      * enclosing instance
      * @param name for debugging
      */
    class Figure(name: String) { d =>

      /**
        * Check if this figure can interact with another figure.
        * That other figure must be of a type that is 
        * a member of the same instance
        * @param other figure to match
        * @return irrelevant
        */
      def canBeat(other: Figure): Boolean = {
        println(this + " beats " + other)
        true
      }
      override def toString: String = s"D$name in $c"
    }

    def canBeat(d1:Figure, d2:Figure): Boolean = d1.canBeat(d2)
  }

  def sameThing[T](arg: T): T = arg
  

  val c0 = new Board("0")
  val d01 = new c0.Figure("01")
  val d02 = new c0.Figure("02")
  val d03 = sameThing(d01)
  val c1 = new Board("1")
  val d11 = new c1.Figure("11")
  val d12 = new c1.Figure("12")
  val d13 = sameThing(d01)

  val c2 = sameThing[c0.type](c0)
  val d21 = new c2.Figure("11")
  val d22 = new c2.Figure("12")
  val d23 = sameThing(d21)

  d01.canBeat(d01)
  d01.canBeat(d02)
  d01.canBeat(d03)

  d02.canBeat(d01)
  d02.canBeat(d02)
  d02.canBeat(d03)

  d03.canBeat(d01)
  d03.canBeat(d02)
  d03.canBeat(d03)

//  d01 canBeat d11  // won't compile
//  d11 canBeat d21  // won't compile
  d23.canBeat(d21)

  def canBeat(c: Board)(d1:c.Figure, d2:c.Figure): Boolean = c.canBeat(d1, d2)
  canBeat(c0)(d01, d02)
  canBeat(c0)(d01, d11)  // won't compile
  canBeat(c0)(d01, d21)  // will compile (why?)
}
