import scala.annotation.tailrec

object MyProgram:
  def abs(n: Int): Int =
    if n < 0 then -n
    else n

  private def formatAbs(x: Int) =
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))

  private def formatFactorial(n: Int) =
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))

  private def formatResult(name: String, n: Int, f: Int => Int) =
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))

  def factorial(n: Int): Int =
    @tailrec
    def go(n: Int, acc: Int): Int =
      if n <= 0 then acc
      else go(n - 1, n * acc)
    go(n, 1)

  def fibonacciNumbers(n: Int): Int = {
    println(n)
    if n <= 3 then 1
    else fibonacciNumbers(n - 1) + fibonacciNumbers(n - 2)
  }

  def fib(n: Int): Int = {
    @tailrec
    def go(
        n: Int,
        acc1: Int,
        acc2: Int
    ): Int = n match {
      case n if n <= 2 => acc2
      case _           => go(n - 1, acc2, acc1 + acc2)
    }
    go(n, 0, 1)
  }

  def findFirst[A](n: Int, arr: Array[A], p: A => Boolean): Int = n match {
    case n if n >= arr.length   => -1
    case n if p(arr(n)) == true => n
    case _                      => findFirst(n + 1, arr, p)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(as: Array[A], n: Int): Boolean = {
      if (n == as.length - 1) then true
      else if gt(as(n), as(n + 1)) then false
      else go(as, n + 1)
    }
    go(as, 0)
  }

//   def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
//     (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
  /*
        n: 5, acc1: 0, acc2: 1
        1) 4, 1, 0 + 1
        2) 3, 1, 1 + 1
        3) 2, 2, 1 + 2
        4) 1, return acc 2 == 3
   */
  // @main def printAbs: Unit =
  //   // println(isSorted(Array(1, 2, 3), _ > _)) // true
  //   // println(isSorted(Array(1, 2, 1), _ > _)) // false
  //   // println(isSorted(Array(3, 2, 1), _ < _)) // true
  //   // println(isSorted(Array(1, 2, 3), _ < _)) // false
  //   val increment: Int => Int = (x: Int) => x + 1
  //   val multiplyBy2: Int => Int = (x: Int) => x * 2
  //   println(compose(multiplyBy2, increment)(3))
