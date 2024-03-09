import scala.collection.immutable.List

private enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case None    => None
    case Some(v) => Some(f(v))

  def getOrElse[B >: A](default: => B): B = this match
    case None    => default
    case Some(v) => v

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def withFilter(f: A => Boolean): Option[A] =
    flatMap(a => if f(a) then Some(a) else None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    for a <- this if f(a) yield a

  def foreach(f: A => Unit): Unit = map(f)

object Option {
  def apply[A](a: A): Option[A] = {
    if (a != null) then Some(a) else None
  }
}

import Option.{None, Some}

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

def lift[A, B](f: A => B): Option[A] => Option[B] =
  (oa: Option[A]) => oa.map(f) // _.map(f)

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a.flatMap(aa => b.map(bb => f(aa, bb)))
  /*
    for {
      aa <- a
      bb <- b
    } yield(f(aa, bb))
   */

/*
  combines a list of Options into one
  Option containing a list of all the Some values in the original list. If the
  original list contains None even once, the result of the function should be
  None;
 */
def sequence[A](as: List[Option[A]]): Option[List[A]] =
  traverse(as)(a => a)
/*
  map over a list using a function that might fail,
  returning None if applying it to any element of the list returns None.
 */
def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
  as.foldRight(Some(Nil): Option[List[B]])((a, acc) =>
    f(a).flatMap((aa) => acc.map((bb) => aa :: bb))
  )

def toIntOption(s: String): Option[Int] =
  try Some(s.toInt)
  catch case _: NumberFormatException => None

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 2.0

def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String
): Option[Double] =
  val optAge: Option[Int] = toIntOption(age)
  val optTickets: Option[Int] = toIntOption(numberOfSpeedingTickets)
  map2(optAge, optTickets)(insuranceRateQuote)

// @main def go(): Unit =
//   val opt = Option(42)
//   opt.foreach(println)
