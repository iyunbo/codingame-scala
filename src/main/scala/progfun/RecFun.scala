package org.iyunbo.coding
package progfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, _) => 1
    case (x, y) if x == y => 1
    case (x, y) => pascal(x - 1, y - 1) + pascal(x, y - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def countPairs(list: List[Char]): Int = {
      list match {
        case List() => 0
        case '(' :: xs => countPairs(xs) + 1
        case ')' :: xs => val count = countPairs(xs); if (count <= 0) count - 1 else Int.MaxValue
        case _ :: xs => countPairs(xs)
      }
    }

    val balance = countPairs(chars)
    balance == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case (0, _) => 1
      case (_, List()) => 0
      case (m, c :: cs) if m == c => countChange(m, cs) + 1
      case (m, c :: cs) if m < c => countChange(m, cs)
      case (m, c :: cs) if m > c => countChange(m - c, c :: cs) + countChange(m, cs)
    }
  }
}
