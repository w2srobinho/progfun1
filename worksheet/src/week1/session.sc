object session {

  object oldSqrt {
    def abs(x: Double) = if (x < 0) -x else x

    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)

    def isGoodEnough(guess: Double, x: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double, x: Double) =
      (guess + x / guess) / 2

    def sqrt(x: Double) = sqrtIter(1.0, x)
  }

  object newSqrt {
    def sqrt(x: Double) = {
      def abs(x: Double) = if (x < 0) -x else x

      def sqrtIter(guess: Double, x: Double): Double =
        if (isGoodEnough(guess, x)) guess
        else sqrtIter(improve(guess, x), x)

      def isGoodEnough(guess: Double, x: Double) =
        abs(guess * guess - x) / x < 0.001

      def improve(guess: Double, x: Double) =
        (guess + x / guess) / 2

      sqrtIter(1.0, x)
    }
  }

  newSqrt.sqrt(2)
  newSqrt.sqrt(4)
  newSqrt.sqrt(1e-6)
  newSqrt.sqrt(0.001)
  newSqrt.sqrt(1e60)

  object recursive {
    def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)

    def factorial(n: Int): Int =
      if (n == 0) 1 else n * factorial(n - 1)
  }

}