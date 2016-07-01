import sun.font.TrueTypeFont



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

def pascal(c: Int, r: Int): Int =
  if(c == 0 || c == r) 1
  else pascal(c-1, r-1) + pascal(c, r-1)

pascal(1,2)

balance("(if (zero? x) max (/ 1 x))".toList)


object session {

  /*Newton method to find srt*/
  def abs(x: Double) = if(x < 0) -x else x
  def meanof2(x: Double, y: Double) = (x+y)/2

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGuessEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def isGuessEnough(guess: Double, x: Double): Boolean =
    abs(guess * guess - x) / x < 0.001

  def improve(guess: Double, x: Double): Double = meanof2(guess, x/guess)

  def sqrt(x: Double): Double = sqrtIter(x/2, x)

  }


object nestedfunction {

  /*Newton method to find srt*/
  def abs(x: Double) = if (x < 0) -x else x
  def sqrt(x: Double): Double = {

    def meanof2(x: Double, y: Double) = (x + y) / 2
    def sqrtIter(guess: Double): Double =
      if (isGuessEnough(guess)) guess
      else sqrtIter(improve(guess, x))
    def isGuessEnough(guess: Double): Boolean =
      abs(guess * guess - x) / x < 0.001
    def improve(guess: Double, x: Double): Double = meanof2(guess, x / guess)

    sqrtIter(1.0)
  }

  sqrt(2)

}