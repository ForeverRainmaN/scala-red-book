package rb.p3

import scala.annotation.tailrec

enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])

object MyList {
  def apply[A](as: A*): MyList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail*))
  }

  def isEmpty[A](as: MyList[A]): Boolean = as match
    case Nil => true
    case _   => false

  def tail[A](as: MyList[A]): MyList[A] = as match
    case Cons(_, tl) => tl
    case Nil         => sys.error("tail	of	empty	list")

  def setHead[A](as: MyList[A], hd: A): MyList[A] = as match
    case Cons(_, tl) => Cons(hd, tl)
    case Nil         => sys.error("setHead	of	empty	list")

  def drop[A](as: MyList[A], n: Int): MyList[A] = as match
    case Nil                      => Nil
    case Cons(_, tail) if (n > 0) => drop(tail, n - 1)
    case _                        => as

  def dropWhile[A](as: MyList[A], f: A => Boolean): MyList[A] = as match
    case Cons(hd, tl) if f(hd) => dropWhile(tl, f)
    case _                     => as

  def sum(ints: MyList[Int]): Int = ints match
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)

  def product(ds: MyList[Double]): Double = ds match
    case Nil         => 1.0
    case Cons(x, xs) => x * product(xs)

  def length[A](as: MyList[A]): Int =
    foldRight(as, 0, (_, acc) => acc + 1)

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = a1 match
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))

  def appendViaFoldLeft[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    foldLeft(reverse(a1), a2, (acc, head) => Cons(head, acc))

  def concatLists[A](ll: MyList[MyList[A]]): MyList[A] =
    foldRight(ll, Nil: MyList[A], append)

  def map[A, B](as: MyList[A])(f: A => B): MyList[B] = {
    @tailrec
    def go(remaining: MyList[A], acc: MyList[B]): MyList[B] = remaining match
      case Nil              => reverse(acc)
      case Cons(head, tail) => go(tail, Cons(f(head), acc))

    go(as, Nil)
  }

  def filter[A](as: MyList[A], f: A => Boolean): MyList[A] = {
    @tailrec
    def go(remaining: MyList[A], acc: MyList[A]): MyList[A] = remaining match
      case Nil                           => reverse(acc)
      case Cons(head, tail) if (f(head)) => go(tail, Cons(head, acc))
      case Cons(head, tail)              => go(tail, acc)

    go(as, Nil)
  }

  def filterViaFlatMap[A](as: MyList[A], predicate: A => Boolean): MyList[A] = {
    flatMap(as, (x: A) => if (predicate(x)) MyList(x) else Nil)
  }

  def zipWithSum(xs: MyList[Int], ys: MyList[Int]): MyList[Int] = {
    def go(xs: MyList[Int], ys: MyList[Int]): MyList[Int] = (xs, ys) match {
      case (Nil, Nil)                   => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, go(t1, t2))
      case (Nil, Cons(h2, t2))          => Cons(h2, go(Nil, t2))
      case (Cons(h1, t1), Nil)          => Cons(h1, go(t1, Nil))
    }
    go(xs, ys)
  }

  def zipWith[A, B, C](as: MyList[A], bs: MyList[B])(f: (A, B) => C): MyList[C] = {
    def go(remainingAs: MyList[A], remainingBs: MyList[B]): MyList[C] =
      (remainingAs, remainingBs) match {
        case (Nil, Nil)                   => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), go(t1, t2))
        case (Nil, _)                     => Nil
        case (_, Nil)                     => Nil
      }

    go(as, bs)
  }

  def flatMap[A, B](as: MyList[A], f: A => MyList[B]): MyList[B] = {
    @tailrec
    def collect(rem: MyList[A], acc: MyList[MyList[B]]): MyList[MyList[B]] = rem match {
      case Nil        => acc
      case Cons(h, t) => collect(t, Cons(reverse(f(h)), acc))
    }

    val reversedLists = collect(as, Nil)

    @tailrec
    def flatten(lists: MyList[MyList[B]], current: MyList[B], result: MyList[B]): MyList[B] =
      (lists, current) match {
        case (Nil, Nil)        => result
        case (Cons(h, t), Nil) => flatten(t, h, result)
        case (_, Cons(h, t))   => flatten(lists, t, Cons(h, result))
      }

    reverse(flatten(reversedLists, Nil, Nil))
  }

  def init[A](as: MyList[A]): MyList[A] = as match
    case Nil          => sys.error("init	of	empty	list")
    case Cons(_, Nil) => Nil
    case Cons(hd, tl) => Cons(hd, init(tl))

  def foldRight[A, B](as: MyList[A], acc: B, f: (A, B) => B): B = as match
    case Nil              => acc
    case Cons(head, tail) => f(head, foldRight(tail, acc, f))

  def foldRightViaFoldLeft[A, B](as: MyList[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (b, a) => f(a, b))

  def foldLeft[A, B](as: MyList[A], acc: B, f: (B, A) => B): B = {
    @tailrec
    def go(remaining: MyList[A], acc: B): B = remaining match
      case Nil              => acc
      case Cons(head, tail) => go(tail, f(acc, head))

    go(as, acc)
  }
  // sadly has a bug :/
  def hasSubsequence[A](as: MyList[A], searchedSubseq: MyList[A]): Boolean = {
    val subseqLength = length(searchedSubseq)

    @tailrec
    def go(remainingMain: MyList[A], remainingSub: MyList[A], matchLength: Int): Boolean =
      if matchLength == subseqLength then true
      else
        (remainingMain, remainingSub) match
          case (Nil, Nil)                   => false
          case (Cons(h1, t1), Cons(h2, t2)) =>
            if (h1 == h2) go(t1, t2, matchLength + 1) else go(t1, remainingSub, 0)
          case (Nil, _) => false
          case (_, Nil) => false

    if (isEmpty(as) || subseqLength == 0) false else go(as, searchedSubseq, 0)
  }

  def sumViaFoldLeft(ns: MyList[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: MyList[Double]): Double =
    foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](as: MyList[A]): Int =
    foldLeft(as, 0, (acc, _) => acc + 1)

  def sumViaFoldRight(ns: MyList[Int]): Int =
    foldRight(ns, 0, (x, y) => x + y)

  def reverse[A](as: MyList[A]): MyList[A] =
    foldLeft(as, Nil: MyList[A], (acc, a) => Cons(a, acc))

  def productViaFoldRight(ns: MyList[Double]): Double =
    foldRight(ns, 1.0, _ * _)
}

object Test extends App {
  import MyList.*

  val testList = MyList(1, 2, 3, 4, 5, 6)
  val newList  = MyList(3, 4)

  val test = appendViaFoldLeft(testList, newList)
  println(hasSubsequence(MyList(1, 1, 2, 3), MyList(1, 2, 3)))
}
