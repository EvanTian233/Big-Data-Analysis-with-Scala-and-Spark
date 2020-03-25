package recfun

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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def counter(chars: List[Char], num: Int): Boolean = {
      if (chars.isEmpty) num == 0
      else if (chars.head == '(') counter(chars.tail, num +1)
      else if (chars.head == ')') num > 0 && counter(chars.tail, num -1)
      else counter (chars.tail, num)
    }
    counter(chars, 0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (money - coins.head == 0) 1 + countChange(money, coins.tail)
    else if (money - coins.head < 0) countChange(money, coins.tail)
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}



