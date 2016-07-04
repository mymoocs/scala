def sum(f: Int => Int, a: Int, b: Int): Int =
  if(a > b) 0 else f(a) +sum(f, a+1, b)

def id(x: Int) = x
def cube = (x: Int) => x*x*x

val ls = List(1,2,3,4)
sum(id, 1, 6)
sum(cube, 1, 6)


def sum1(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a+1, acc+f(a))
  }
  loop(a, 0)
}

def sumCube(a:Int, b: Int) = sum1(cube)
sumCube(1,6)
sumCube(1,7)




