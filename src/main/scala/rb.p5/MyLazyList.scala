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
      case Cons(h, t) =>
        val hv = h()
        if (p(hv)) go(t(), hv :: acc) else LazyList(acc.reverse*)

    go(this, List.empty)
  }

  def headOptionV2: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def takeWhileV3(p: A => Boolean): LazyList[A] =
    foldRight(Empty: LazyList[A])((a, b) => if (p(a)) LazyList.cons(a, b) else Empty)

  def exists(p: A => Boolean): Boolean = this match
    case Cons(h, t) => p(h()) || t().exists(p)
    case _          => false

  def existsV2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = this match
    case Empty      => true
    case Cons(h, t) => p(h()) && t().forAll(p)

  def forAllV2(p: A => Boolean) =
    foldRight(true)((a, b) => p(a) && b)

  def foldRight[B](acc: => B)(f: (A, => B) => B): B =
    this match
      case Cons(h, t) => f(h(), t().foldRight(acc)(f))
      case _          => acc

  def mapViaFoldRight[B](f: A => B): LazyList[B] =
    foldRight(Empty: LazyList[B])((a, b) => LazyList.cons(f(a), b))

  def filterViaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(Empty: LazyList[A])((a, b) => if (p(a)) LazyList.cons(a, b) else b)

  def appendViaFoldRight[B >: A](other: => LazyList[B]): LazyList[B] =
    foldRight(other)((h, t) => LazyList.cons(h, t))

  def flatMapViaFoldRight[B >: A](f: A => LazyList[B]): LazyList[B] =
    foldRight(Empty: LazyList[B])((a, b) => f(a).appendViaFoldRight(b))
}

object LazyList {
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: LazyList[A] = Empty

  def onesViaUnfold: LazyList[Int] = unfold(1)(one => Some((one, one)))

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(a)(s => Some((s, s)))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n + 1))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def fibs: LazyList[Int] =
    def go(a: Int, b: Int): LazyList[Int] =
      LazyList.cons(a, go(b, a + b))
    go(0, 1)

  def fibsViaUnfold: LazyList[Int] = unfold((0, 1)) { (a, b) => Some((a, (b, a + b))) }

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((v, nextState)) => LazyList.cons(v, unfold(nextState)(f))
      case None                 => LazyList.empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))
}
