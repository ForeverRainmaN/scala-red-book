package rb.p2

import scala.annotation.tailrec

object exercises {
  /*
    Write	a	recursive	function	to	get	the	nth	Fibonacci	number
(http://mng.bz/C29s).	The	first	two	Fibonacci	numbers	are	0	and	1.	The
nth	number	is	always	the	sum	of	the	previous	two—the	sequence	begins
0,	1,	1,	2,	3,	5.	Your	definition	should	use	a	local,	tail-recursive

   */

  def fib(n: Int): Int = {
    if (n == 0) n
    else if (n < 2) 1
    else fib(n - 1) + fib(n - 2)
  }

  def fibTailRec(n: Int): Long = {
    @tailrec
    def fibHelper(count: Int, current: Long, previous: Long): Long = {
      if (count == 0) previous
      else fibHelper(count - 1, previous + current, current)
    }
    require(n >= 0, "n must be non-negative")
    fibHelper(n, 1, 0)
  }

  def fib2(n: Int): Int =
    @annotation.tailrec
    def go(n: Int, current: Int, previous: Int): Int =
      if n <= 0 then current
      else go(n - 1, previous, current + previous)
    go(n, 0, 1)
  /*
        10, 1 + 0, 1
        9, 1 + 1, 1
        8, 2 + 1, 2
        7, 3 + 2, 3
        6, 5 + 3, 5
        5, 8 + 5, 8
        4, 13 + 8, 13
        3, 21 + 13, 21
        2, 34 + 21, 34
        1, 55 + 34, 55
   */

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    def go(i: Int): Boolean = {
      if (i >= as.length - 1) true
      else if (gt(as(i), as(i + 1))) false
      else go(i + 1)
    }

    go(0)
  }

  def area(width: Double, height: Double): Double = width * height
  def areaOfTen                                   = partial1(10.0, area)

  // returns of function that takes (b: Double) => area(10.0, b)

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

}

object Test extends App {
  println(exercises.fibTailRec(10))
  println(exercises.isSorted(Array(1, 2, 3), _ > _))
  println(exercises.isSorted(Array(1, 2, 1), _ > _))
  println(exercises.isSorted(Array(3, 2, 1), _ < _))
  println(exercises.isSorted(Array(1, 2, 3), _ < _))
}
