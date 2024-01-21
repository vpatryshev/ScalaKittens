package scalakittens.la

object Nat {
  trait NAT[N] { val nat: Int }
  def NAT[N: NAT]: NAT[N] = implicitly
  def nat[N: NAT]: Int = NAT[N].nat

  trait NAT_10
  implicit object NAT_10 extends NAT[NAT_10]
  { override val nat = 10 }

  trait NAT_100
  implicit object NAT_100 extends NAT[NAT_100]
  { override val nat = 100 }

  case class Fin[N: NAT](i: Int) { // 0 <= i < nat[N]
    final val max = nat[N]
  }
}
