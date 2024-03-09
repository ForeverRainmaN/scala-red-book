package com.scala.red.book.chapter.four
import scala.util.Either

object Options extends App {
  val anOption: Option[Int] = Option(42)
  val anOption2: Option[Int] = Option(28)
  val anOption3: Option[Int] = Option(93)

  val res = for {
    a <- anOption
    b <- anOption2
    c <- anOption3
  } yield a + b + c

  def parseIntsToString(as: List[String]): Option[List[Int]] =
    Option(as.map(_.toInt))

  def lift[A, B](f: (A, A) => B): (Option[A], Option[A]) => Option[B] =
    (op1: Option[A], op2: Option[A]) =>
      for {
        op1Value <- op1
        op2Value <- op2
      } yield f(op1Value, op2Value)

  val intToString: (Int, Int) => String = (x, y) => s"$x, $y"

  val lifted = lift(intToString)

}
