import sun.font.TrueTypeFont

object session {
  def sqrt(x: Double): Double = 0.0

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGuessEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def isGuessEnough(g: Double, x: Double): Boolean = true

  def improve(g: Double, x: Double): Double = 0.0

  1+2+4
  
  }