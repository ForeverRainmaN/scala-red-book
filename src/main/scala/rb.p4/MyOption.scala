package rb.p4
import scala.annotation.tailrec

enum Option[+A]:
  case Some(v: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case Some(v) => Some(f(v))
    case None    => None

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match
    case Some(v) => v
    case None    => default

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    map(v => if f(v) then Some(v) else None).getOrElse(None)

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _.map(f)

  def toIntOption(s: String): Option[Int] =
    try Some(s.toInt)
    catch case _: NumberFormatException => None

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap: aa =>
      b.map: bb =>
        f(aa, bb)

  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def go(acc: List[A], remaining: List[Option[A]]): Option[List[A]] = {
      if (remaining.isEmpty) Some(acc.reverse)
      else
        remaining.head match
          case Some(v) => go(v :: acc, remaining.tail)
          case None    => None
    }
    go(List.empty, as)
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def go(remaining: List[A], acc: List[B]): Option[List[B]] = {
      if (remaining.isEmpty) Some(acc)
      else
        f(remaining.head) match
          case Some(v) => go(remaining.tail, v :: acc)
          case None    => None
    }
    go(as, List.empty)
  }

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] =
    val optAge: Option[Int]     = toIntOption(age)
    val optTickets: Option[Int] = toIntOption(numberOfSpeedingTickets)

    map2(optAge, optTickets)(insuranceRateQuote)

object MyOption extends App {
  import Option.*

  val asd      = Some(2)
  val filtered = asd.filter(_ < 5)

  println(filtered)

}
