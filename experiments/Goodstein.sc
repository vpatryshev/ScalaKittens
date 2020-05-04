type BI = BigInt

def pow(k: BI, p: BI): BI = {
  if (p == BigDecimal(0)) 1 else {
    val k0: BI = if (p % 2 == 1) k else 1
    val k1 = pow(k, p / 2)
    k0 * k1 * k1
  }
}

val n = 4
val nn = pow(n, n)
val nnn1 = pow(n, nn + 1)
val nn1 = pow(n, n + 1)
val n4 = nn1 + nnn1 + 2


