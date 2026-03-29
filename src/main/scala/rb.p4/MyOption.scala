package rb.p4

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

object MyOption extends App {
  import Option.*

  val asd      = Some(2)
  val filtered = asd.filter(_ < 5)

  println(filtered)

}
