package rb.p5

import scala.annotation.tailrec

enum LazyList[+A] {
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])
  def headOption: Option[A] = this match
    case Empty      => None
    case Cons(h, _) =>
      Some(h())

  def tailOption: Option[LazyList[A]] = this match
    case Empty      => None
    case Cons(_, t) => Some(t())

  def toList: List[A] = {
    @tailrec
    def go(remaining: LazyList[A], acc: List[A]): List[A] = remaining match
      case Empty      => acc.reverse
      case Cons(h, t) => go(t(), h() :: acc)

    go(this, List.empty)
  }

  def take(n: Int): LazyList[A] = this match
    case Empty      => Empty
    case Cons(h, t) =>
      if n <= 0 then Empty
      else LazyList.cons(h(), t().take(n - 1))

  def drop(n: Int): LazyList[A] = this match
    case Empty      => Empty
    case Cons(_, t) =>
      if (n <= 0) this
      else t().drop(n - 1)

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Empty      => Empty
    case Cons(h, t) => {
      val head = h()
      if (p(head)) LazyList.cons(head, t().takeWhile(p))
      else Empty
    }

  def takeWhileV2(p: A => Boolean): LazyList[A] = {
    @tailrec
    def go(remaining: LazyList[A], acc: List[A]): LazyList[A] = remaining match
      case Empty      => LazyList(acc.reverse*)
      case Cons(h, t) => if (p(h())) go(t(), h() :: acc) else LazyList(acc.reverse*)

    go(this, List.empty)
  }
}

object LazyList {
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))
}
