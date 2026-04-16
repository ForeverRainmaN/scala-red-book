package rb.p4

import scala.util.control.NonFatal

enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Left(error)  => Left(error)
    case Right(value) => Right(f(value))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(error)  => Left(error)
    case Right(value) => f(value)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Left(error)  => b
    case Right(value) => Right(value)

  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(x => that.map(y => f(x, y)))

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] = {
    def go(acc: List[A], remaining: List[Either[E, A]]): Either[E, List[A]] = remaining match
      case head :: next =>
        head match
          case Left(value)  => Left(value)
          case Right(value) => go(value :: acc, next)

      case Nil => Right(acc.reverse)

    go(List.empty, as)
  }

  def map2All[E, A, B, C](
      a: Either[List[E], A],
      b: Either[List[E], B],
      f: (A, B) => C
  ): Either[List[E], C] =
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(es), Right(_))   => Left(es)
      case (Right(_), Left(es))   => Left(es)
      case (Left(es1), Left(es2)) => Left(es1 ++ es2)

  def map2Both[E, A, B, C](
      a: Either[E, A],
      b: Either[E, B],
      f: (A, B) => C
  ): Either[List[E], C] =
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(e), Right(_))    => Left(List(e))
      case (Right(_), Left(e))    => Left(List(e))
      case (Left(e1), Left(e2))   => Left(List(e1, e2))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(List.empty[B]): Either[E, List[B]]) { (a, acc) =>
      (f(a), acc) match
        case (Right(b), Right(bs)) => Right(b :: bs)
        case (Left(e), _)          => Left(e)
        case (_, Left(e))          => Left(e)
    }

  def traverseAll[E, A, B](
      as: List[A],
      f: A => Either[List[E], B]
  ): Either[List[E], List[B]] =
    as.foldRight(Right(Nil): Either[List[E], List[B]])((a, acc) => map2All(f(a), acc, _ :: _))

object Either {
  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)
}
