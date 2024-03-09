import scala.Option
import scala.collection.immutable.List
import scala.util.Either
import scala.annotation.tailrec

private enum LazyList[+A]:
  import LazyList.{cons, empty}

  private[this] case Empty
  private[this] case Cons(h: () => A, t: () => LazyList[A])

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h, _) => {
      Some(h())
    }

  def toList: List[A] =
    @tailrec
    def go(acc: List[A], lazyList: LazyList[A]): List[A] =
      lazyList match
        case Empty      => acc
        case Cons(h, t) => go(acc :+ h(), t())
    go(Nil, this)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _                    => empty

  @tailrec
  final def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Empty                => empty
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case Cons(_, t)           => t().takeWhile(p)

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](acc: => B)(f: (A, => B) => B): B =
    this match
      case Cons(h, t) => f(h(), t().foldRight(acc)(f))
      case _          => acc

  def append[B >: A](that: => LazyList[B]): LazyList[B] =
    foldRight(that)((a, acc) => cons(a, acc))

  def flatMap[B >: A](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty: LazyList[B])((a, acc) => f(a).append(acc))

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty: LazyList[B])((a, acc) => cons(f(a), acc))

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => empty

  def mapWithUnfold[B](f: A => B): LazyList[B] =
    unfold(this):
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)):
      case (Cons(h, t), 1)          => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
      case _                        => None

  def zipWith[B, C](
      that: LazyList[B],
      f: (A, B) => C
  ): LazyList[C] =
    unfold((this, that)):
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)):
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) =>
        Some((Some(h1()) -> None) -> (t1() -> Empty))
      case (Empty, Cons(h2, t2)) =>
        Some((None -> Some(h2())) -> (Empty -> t2()))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()) -> Some(h2())) -> (t1() -> t2()))

  def tails: LazyList[LazyList[A]] =
    unfold(this):
      case Empty      => None
      case Cons(h, t) => Some((Cons(h, t), t()))
    .append(LazyList(empty))

  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
    foldRight(init -> LazyList(init)): (a, b0) =>
      lazy val b1 = b0
      val b2 = f(a, b1(0))
      (b2, cons(b2, b1(1)))
    .apply(1)

  def startsWith[A](prefix: LazyList[A]): Boolean =
    this.zipWith(prefix, (_, _)).forAll(_ == _)

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] =
    unfold(this):
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _                    => None

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty: LazyList[A])((a, acc) =>
      if p(a) then cons(a, acc) else acc
    )

  def headOptionWithFRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

object LazyList:
  def cons[A](
      hd: => A,
      tl: => LazyList[A]
  ): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then Empty
    else cons(as.head, apply(as.tail*))

  def continually[A](a: A): LazyList[A] =
    lazy val single: LazyList[A] = cons(a, single)
    single

  def from(n: Int): LazyList[Int] =
    lazy val element: LazyList[Int] = cons(n, from(n + 1))
    element

  val fibs: LazyList[Int] =
    def go(current: Int, next: Int): LazyList[Int] =
      cons(current, go(next, current + next))
    go(0, 1)

  val fibsWithUnfold: LazyList[Int] =
    unfold((0, 1)) { case (current, next) =>
      Some((current, (next, current + next)))
    }

  def fromWithUnfold(n: Int): LazyList[Int] =
    unfold(n)(n => Some(n, n + 1))

  def continuallyWithUnfold[A](a: A): LazyList[A] =
    unfold(())(_ => Some(a, ()))

  val onesWithUnfold: LazyList[Int] =
    unfold(())(_ => Some(1, ()))

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => empty

@main def gogo(): Unit =
  val asd = LazyList.continually(10).take(10).toList
  val qwe = LazyList.continuallyWithUnfold(10).take(10).toList
  val zxc = LazyList.onesWithUnfold.take(10).toList
  val iop = LazyList.fromWithUnfold(0).take(10).toList
  List
  val testll1 = LazyList(1, 2, 3, 4, 5)
  val prefix = LazyList(1, 2, 3)
  val testll2 = LazyList("govno", "zalupa", "penis", "her", "davalka")
  val zipped = testll1.zipWith(testll2, (x, y) => (x, y))
  println(testll1.tails.take(10).toList.foreach(x => x.toList.foreach(println)))
  // println(asd)
  // println(qwe)
  // println(zxc)
  // println(iop)
