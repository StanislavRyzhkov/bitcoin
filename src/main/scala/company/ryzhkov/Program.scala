package company.ryzhkov

import scala.math._

object Program extends App {
  case class Curve(p: BigInt, a: BigInt, b: BigInt)
  object Curve {
    val BitcoinCurve = Curve(
      p = BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F", 16),
      a = BigInt(0),
      b = BigInt(7)
    )
  }

  /** BigInteger point on a curve
    */
  sealed trait Point {
    import Point._

    def +(that: Point): Point = {
      def calculate(m: BigInt, self: N, other: N) = {
//        println(s"m: $m")
        val rx = mod((m * m - self.x - other.x), self.curve.p)
//        println(s"rx: $rx")
        val ry = mod((-(m * (rx - self.x) + self.y)), self.curve.p)
//        println(s"ry: $ry")
        N(self.curve, rx, ry)
      }

      (this, that) match {
        case (Z, Z)                                                            => Z
        case (Z, other)                                                        => other
        case (self, Z)                                                         => self
        case (self: N, other: N) if (self.x == other.x) && (self.y != other.y) => Z
        case (self: N, other: N) if self.x == other.x =>
          val m = (3 * self.x * self.x + self.curve.a) * inv(2 * self.y, self.curve.p)
          calculate(m, self, other)
        case (self: N, other: N) =>
//          println("Other")
          val m = (self.y - other.y) * inv(self.x - other.x, self.curve.p)
          println(s"self-x: ${self.x}")
          println(s"other-x: ${other.x}")
          println(s"self-curve-p: ${self.curve.p}")
          println(s"inv: ${inv(self.x - other.x, self.curve.p)}")
          println(s"M: $m")
          calculate(m, self, other)
      }
    }

    // Returns (gcd, x, y) s.t. a * x + b * y == gcd.
    // This function implements the extended Euclidean algorithm and runs
    // in O(log b) in the worst case, taken from Wikipedia.
    private def extendedEuclideanAlgorithm(a: BigInt, b: BigInt) = {
      var r = (a, b)
      var s = (BigInt(1), BigInt(0))
      var t = (BigInt(0), BigInt(1))

      while (r._2 != 0) {
        val quotient = r._1 / r._2
        r = (r._2, r._1 - quotient * r._2)
        s = (s._2, s._1 - quotient * s._2)
        t = (t._2, t._1 - quotient * t._2)
      }
      (r._1, s._1, t._1)
    }

    // returns modular multiplicate inverse m s.t. (n * m) % p == 1
    private def inv(n: BigInt, p: BigInt) = {
      val (_, x, _) = extendedEuclideanAlgorithm(n, p)
      println(s"x: $x")
      println(s"p: $p")
      val res = mod(x, p)
      println(s"res: $res")
      res
    }

    private def sign(x: BigInt) =
      if (x < 0) -1
      else if (x == 0) 0
      else 1

    private def mod(n: BigInt, d: BigInt): BigInt = {
      val result = n % d
      if (sign(result) * sign(d) < 0) result + d
      else result
    }
  }
  object Point {
    case class N(
        curve: Curve,
        x: BigInt,
        y: BigInt
    ) extends Point

    case object Z extends Point

    val G: Point = N(
      curve = Curve.BitcoinCurve,
      x = BigInt("79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798", 16),
      y = BigInt("483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8", 16)
    )
  }

  case class Generator(
      G: Point,
      n: BigInt
  )
  object Generator {
    val BitcoinGen = Generator(
      G = Point.G,
      n = BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141", 16)
    )
  }

//  val secretKey = {
//    val x = BigInt(256, scala.util.Random)
//    if (x == 0) 1 else x
//  }

  // yˆ2 - xˆ3 - 7 = 0
  // (yˆ2 - xˆ3 - 7) % p = 0
  //  val result = (((G.y * G.y) - (G.x * G.x * G.x)) - 7) % Curve.BitcoinCurve.p == 0
  //  println(s"Generator IS on the curve: $result")

//  val sk1 = 1
//  val pk1 = Point.G
//  pk1 match {
//    case Point.N(curve, x, y) =>
//      println(s"Public key: ${(x, y)}")
//      val ver = (y * y - x * x * x - 7) % curve.p == 0
//      println(s"Verify: $ver")
//    case Point.Z =>
//      println("Error")
//  }

  println(Point.G)
  println("=====")
  println(Point.G + Point.G)
  println("=====")
  println(Point.G + Point.G + Point.G)
}
