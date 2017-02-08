import scala.annotation.tailrec

object session {
    def abs(x: Double): Double =
        if (x > 0) x else -x

    def sqrt(x: Double) = {
        def sqrtIter(guess: Double): Double =
            if (isGoodEnough(guess)) guess
            else sqrtIter(improveGuess(guess))

        def isGoodEnough(guess: Double): Boolean =
            abs(guess * guess - x) < (x * 0.00001)

        def improveGuess(guess: Double): Double =
            (guess + x / guess) / 2

        sqrtIter(1)
    }

    def factorial(x: Int): Int = {
        if (x==0) 1
            else x * factorial(x-1)
    }

    def fact(x: Int): Int = {
        @tailrec
        def fact2(x: Int, acc: Int): Int =
            if (x == 0) acc
            else fact2(x-1, acc*x)
    
        fact2(x, 1)
    }


    def main(args: Array[String]) {
        println(fact(4))
    }
}
