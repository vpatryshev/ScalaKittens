package org.scalakittens

/**
 * A smarter enumeration
 */
abstract class RichEnumeration extends Enumeration {
  import RichEnumeration._

  def enum(i:Int, text:String): EnumVal = {
    val names = (text split ";") .toList map (_.trim)
    new EnumVal(i, names.head, names.tail)
  }

  lazy val allValues:Set[EnumVal] = {
    val setOfValues: Set[Value] = super.values
    setOfValues collect { case x:  EnumVal ⇒ x}
  }

  def find(text:String):Option[EnumVal] = {
    for {c <- comparators
         v <- allValues
    } if (v.matches(c)(text)) return Some(v)

    None
  }

  class EnumVal(i:Int, val name: String, val alternatives:List[String] = Nil) extends Val(i, name) with Choice {
    lazy val fullList: List[String] = name::alternatives

    def matches(c:Comparator)(s:String): Boolean = fullList.map(c).exists(_(s))
    def matches(s:String): Boolean = fullList.map(fully).exists(_(s))
    def isKnown: Boolean = id != 0
  }
}

object RichEnumeration {
  type Comparator = String⇒String⇒Boolean

  private val fully    : Comparator = (x:String) ⇒ (y:String) ⇒ x equalsIgnoreCase y
  private val partially: Comparator = (x:String) ⇒ (y: String) ⇒ y.toLowerCase contains x.toLowerCase

  //comparators.exists(c ⇒
  val comparators:List[Comparator] = List(fully, partially)

  trait Choice {
    def name: String
    def matches(s:String): Boolean
    def isKnown: Boolean
    def isUnknown: Boolean = !isKnown
  }

}
