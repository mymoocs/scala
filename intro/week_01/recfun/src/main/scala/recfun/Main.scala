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
      if(c == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
    def loop(xs: List[Char], l: Int): Boolean =
      if(xs.isEmpty) true && l == 0
      else
        xs match {
        case '('::ys => loop(ys,l+1)
        case ')'::ys => if(l == 0) false else loop(ys,l-1)
        case  _ ::ys => loop(ys,l)
      }

    loop(chars,0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int =
      if(money == 0 || coins.isEmpty) 0
      else coins match {
        case h::ts => if(money > h) countChange(money-h,ts) else countChange(money, ts)
      }

  }
