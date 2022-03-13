package company.ryzhkov

object Test extends App {
  def sign(x: BigInt) =
    if (x < 0) -1
    else if (x == 0) 0
    else 1

  def mod(n: BigInt, d: BigInt): BigInt = {
    val result = n % d
    if (sign(result) * sign(d) < 0) result + d
    else result
  }

  val x = BigInt("-37732016455592228448156445462498416408852156224504820822700452822953572406827")
//  val x = BigInt("-11")
//  println(x)
  val y = BigInt("115792089237316195423570985008687907853269984665640564039457584007908834671663")
//  val y = BigInt("2")
//  println(y)
  val z = mod(x, y)

  println(z)
}
