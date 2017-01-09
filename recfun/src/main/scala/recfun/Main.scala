package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println "Pascal's Triangle"
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c==0) 1
    else
      if (r == 0) 0
      else pascal(c, r - 1) + pascal(c - 1, r - 1)


  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      @tailrec
      def f(num_open: Int, chars: List[Char]): Boolean = {
        if (chars.isEmpty) num_open == 0
        else {
          val n = chars.head match {
            case '(' => 1
            case ')' => -1
            case _ => 0
          } + num_open
          if (n < 0) false
          else f(n, chars.tail)
        }
      }
      f(0, chars)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(money > 0 && coins.nonEmpty)
        countChange(money-coins.head, coins) + countChange(money, coins.tail)
      else 0
    }
  }
