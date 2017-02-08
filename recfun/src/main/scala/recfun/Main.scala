package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = 
      if (c == 0 || c == r) 1
      else pascal(c, r-1) + pascal(c-1, r-1) 
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
        @tailrec
        def iter(chars: List[Char], paren_count: Int): Boolean = {
            if (chars.isEmpty || paren_count < 0)
                paren_count == 0
            else {
                if (chars.head == '(')
                    iter(chars.tail, paren_count + 1)
                else if (chars.head == ')')
                    iter(chars.tail, paren_count - 1)
                else
                    iter(chars.tail, paren_count)
            }

        }

        iter(chars, 0)
    }
        
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
        def iter(money: Int, coins: List[Int], count: Int): Int = {
            if (money == 0)
                1
            else if (coins.isEmpty)
                0
            else if (money >= coins.head)
                iter(money-coins.head, coins, count) + iter(money, coins.tail, count)
            else
                iter(money, coins.tail, count)
        }
        iter(money, coins, 0)
    }
  }
