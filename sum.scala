def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a+1, f(a) + acc)
  }
  loop(a,0)
}

def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
}

def fact(n: Int):Int =
    product(x => x)(1,n)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, unit: Int)(a: Int, b: Int):Int = {
    if (a>b) unit
    else combine(f(a), mapReduce(f, combine, unit)(a + 1, b))
}

import math.abs

def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < 0.0001

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
        val next = f(guess)
        if (isCloseEnough(guess, next)) next
        else iterate(next)
    }
    iterate(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double) = (x + f(x))/2


def sqrt(x: Double) = 
    fixedPoint(averageDamp(a => x/a))(1.0)

//println(sum((x: Int) => x, 1, 3))
//println(product((x: Int)=> x)( 1,3))
//println(fact(3))
//print(mapReduce(x => x, (a, b) => a * b, 1)(1, 3))
println(sqrt(2.0))
