package rb.p2

import scala.annotation.tailrec

object PureFunctionsAndHOFs {

  def findFirst[A](as: Array[A], p: A => Boolean): Int =
    @tailrec
    def loop(n: Int): Int =
      if n >= as.length then -1
      else if p(as(n)) then n
      else loop(n + 1)

    loop(0)

  def intPredicate(v: Int): Boolean = {
    if (v == 5) true else false
  }

  findFirst(Array(1, 2, 3, 4, 5, 6), (x) => x > 5)
}
