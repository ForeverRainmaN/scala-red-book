import scala.annotation.tailrec

enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil

  /* Another data constructor, representing nonempty lists. Note that `tail` is
   another `List[A]`, which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def head[A](xs: List[A]): A = xs match
    case Nil        => sys.error("List is empty")
    case Cons(h, _) => h

  def tail[A](xs: List[A]): List[A] = xs match
    case Nil        => sys.error("List is empty")
    case Cons(_, t) => t

  def appendList[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys, (x, acc) => Cons(x, acc))
    /*
      [1,2,3] :: [4,5,6]
      -> 1 append([5,6], [4,5,6])
      -> 2 append([6, nill], [4,5,6])
      -> 3 append([], [4,5,6])
     */
  def concatListOfLists[A](xs: List[List[A]]): List[A] =
    foldRight(
      xs,
      Nil: List[A],
      (x, acc) => appendList(x, acc)
    )

  def doubleToString(xs: List[Double]): List[String] = xs match
    case Nil        => Nil
    case Cons(h, t) => Cons(h.toString(), doubleToString(t))

  def map[A, B](as: List[A], f: A => B): List[B] =
    foldRight(as, Nil: List[B], (x, acc) => Cons(f(x), acc))

  def flatmap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldLeft(as, Nil: List[B], (x, acc) => appendList(acc, f(x)))

  def filter[A](as: List[A], p: A => Boolean): List[A] = as match
    case Nil => Nil
    case Cons(h, t) => {
      val filteredTail = filter(t, p)

      if p(h) then Cons(h, filteredTail) else filteredTail
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???

  def combineTwoIntLists[A](xs: List[Int], ys: List[Int]): List[Int] = xs match
    case Nil         => Nil
    case Cons(x, xs) => Cons(x + head(ys), combineTwoIntLists(xs, tail(ys)))

  def plusOne[A](xs: List[Int]): List[Int] = xs match
    case Nil        => Nil
    case Cons(h, t) => Cons(h + 1, plusOne(t))

  def plusOne2[A](xs: List[Int]): List[Int] =
    foldRight(xs, Nil: List[Int], (x, acc) => Cons(x + 1, acc))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))
  /*
    1  + foldRight(Cons(2, Cons(3, Nil))), 0, (x, y) => x + y)
    1 + (2 + foldRight(Cons(3, Nil), 0, (x, y) => x + y)
    1 + (2 + (3 + (foldRight(Nil, 0, (x, y) => x + y))))
    1 + (2 + (3 + (0)))
   */

  def foldRight2[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, f)

  def foldRight3[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(as, (b: B) => b, (a, g) => b => g(f(a, b)))(acc)

  /*  */

  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B = as match
    case Nil        => acc
    case Cons(h, t) => foldLeft(t, f(h, acc), f)

  def sum(ns: List[Int]) =
    foldLeft(ns, 0, _ + _)

  def subtract(ns: List[Int]) =
    foldLeft(ns, 0, _ - _)

  def product(ns: List[Double]) =
    foldLeft(ns, 0.1, _ * _)

  def length[A](as: List[A]): Int =
    foldLeft(as, 0, (_, acc) => acc + 1)

  def reverse[A](as: List[A]) =
    foldLeft(as, Nil: List[A], (a, acc) => Cons(a, acc))

// add to start
  def prepend[A](xs: List[A], x: A): List[A] = xs match
    case Nil        => Cons(x, Nil)
    case Cons(h, t) => Cons(x, prepend(t, h))

// add to end
  def append[A](xs: List[A], x: A): List[A] = xs match
    case Nil        => Cons(x, Nil)
    case Cons(h, t) => Cons(h, append(t, x))

  def toString[A](as: List[A]): String =
    @tailrec
    def go(as: List[A], acc: String): String = as match
      case Nil          => acc
      case Cons(h, Nil) => acc ++ s"$h"
      case Cons(h, t)   => go(t, acc ++ s"$h,")

    s"[${go(as, "")}]"

  def setHead[A](as: List[A], newHead: A): List[A] = as match
    case Nil           => Cons(newHead, Nil)
    case Cons(_, tail) => Cons(newHead, tail)

  @tailrec
  def drop[A](as: List[A], n: Int): List[A] =
    if n <= 0 then as
    else
      as match
        case Cons(_, t) => drop(t, n - 1)
        case Nil        => Nil

  @tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _                  => as

  def init[A](as: List[A]): List[A] = as match
    case Nil          => sys.error("Init on empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))

// @main def gogo: Unit =
//   val list = List[Int](1, 2, 3)
//   val list2 = List[Int](4, 5, 6)

//   println(List.combineTwoIntLists(list, list2))
// println(List.map(list, i => i + 1))
// println(List.filter(list, _ > 5))
// println(List.map(list, _ + 1))
// println(List.appendList(list, list2))
// println(List.foldLeft(list, "", (x, acc) => acc ++ " " ++ s"${x}"))
// println(List.foldRight(list, "", (x, acc) => acc ++ " " ++ s"${x}"))
