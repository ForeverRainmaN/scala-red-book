import scala.util.control.NonFatal
import scala.collection.immutable.List

import scala.util.Try

private enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Left(value)  => Left(value)
    case Right(value) => Right(f(value))

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Left(_)  => b
    case Right(v) => Right(v)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(value)  => Left(value)
    case Right(value) => f(value)

  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for
      a <- this
      b <- that
    yield f(a, b)

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverse(as)(a => a)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]])((a, acc) =>
      for
        aValue <- f(a)
        accValue <- acc
      yield aValue :: accValue
    )

import Either.{Left, Right}

object Either {
  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)
}

// def mean(xs: Seq[Double]): Either[String, Double] =
//   if xs.isEmpty then Left("mean of empty list!")
//   else Right(xs.sum / xs.length)

def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
  try Right(x / y)
  catch case NonFatal(t) => Left(t)

// example
case class Name private (value: String)

object Name:
  def apply(name: String): Either[String, Name] =
    if name == "" || name == null then Left("Name is empty.")
    else Right(new Name(name))

case class Age private (value: Int)

object Age:
  def apply(age: Int): Either[String, Age] =
    if age < 0 then Left("Age is out of range.")
    else Right(new Age(age))

case class Person(name: Name, age: Age)

object Person:
  def makeBoth(name: String, age: Int): Either[List[String], Person] =
    map2Both(Name(name), Age(age), Person(_, _))

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

def traverseAll[E, A, B](
    as: List[A],
    f: A => Either[List[E], B]
): Either[List[E], List[B]] =
  as.foldRight(Right(Nil): Either[List[E], List[B]])((a, acc) =>
    for {
      aa <- f(a)
      accV <- acc
    } yield aa :: accV
  )

// @main def go(): Unit =
//   val p1 = Person.makeBoth("Curry", 34)
//   val p2 = Person.makeBoth("Howard", 44)
//   val pair = map2Both(p1, p2, (_, _))

//   println(pair)
