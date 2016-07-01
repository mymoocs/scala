package recfun

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
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(xs: List[Char], l: Int): Boolean =
        xs match {
          case Nil => l == 0
          case '(' :: ys => loop(ys, l + 1)
          case ')' :: ys => if (l == 0) false else loop(ys, l - 1)
          case _ :: ys => loop(ys, l)
        }

    loop(chars, 0)
  }

  /**
    * Exercise 3: Counting Change
    * Write a recursive function that counts how many different ways you can make
    * change for an amount, given a list of coin denominations. For example,
    * there are 3 ways to give change for 4 if you have coins with denomiation
    * 1 and 2: 1+1+1+1, 1+1+2, 2+2.
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def change(m: Int, xs: List[Int]): Int =
      if (m == 0) 1
      else if (m < 0) 0
      else xs match {
        case Nil => 0
        case h :: ts => change(m - h, xs) + change(m, ts)
      }
    //just handle zero money case
    if (money == 0) 0
    else change(money, coins)
  }

}


