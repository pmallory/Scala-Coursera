def isPrime(n: Int): Boolean =
    (2 until n) forall (n % _ != 0)

/*
println(isPrime(2))
println(isPrime(3))
println(isPrime(4))
println(isPrime(5))
println(isPrime(6))
*/

def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for { x <- xs
          y <- ys
    } yield x*y).sum


