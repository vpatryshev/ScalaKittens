abstract class Nat[N] { val nat: Int }

def nat[N: Nat]: Int = implicitly[Nat[N]].nat

class VS[N: Nat] {
  val dim = nat[N]
  
  trait V extends (Int => Double) {
    
  }
  
  class Vf(private val f: Int => Double) extends V {
    def apply(i: Int) = f(i)
  }
  
  class Va(private val a: Array[Double]) extends V {
    def apply(i: Int) = a(i)
  }
}

object VST {
  def dim(n: Int) = new class N extends Nat[N] {
    val nat = n
  }
}
