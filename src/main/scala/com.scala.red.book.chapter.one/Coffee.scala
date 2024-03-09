package com.scala.red.book.chapter.three

// first example
// class Cafe:
//   def buyCoffee(cc: CreditCard): Coffee =
//     val cup = Coffee()
//     cc.charge(cup.price)
//     cup

// class CreditCard:
//   def charge(price: Double): Unit =
//     println("charging " + price)

// class Coffee:
//   val price: Double = 2.0

// second example (updated)

// class Cafe2:
//   def buyCoffee(cc: CreditCard, p: Payments): Coffee =
//     val cup = Coffee()
//     p.charge(cc, cup.price)
//     cup

// class CreditCard

// trait Payments:
//   def charge(cc: CreditCard, price: Double): Unit

// class SimulatedPayments extends Payments:
//   def charge(cc: CreditCard, price: Double): Unit =
//     println("charging " + price + " to " + cc)

// class CreditCard2

// val cc = CreditCard()
// val p = Payments()
// val cafe = Cafe2()
// val cup = cafe.buyCoffee(cc, p)

// functional solution:

class CreditCard:
  def charge(price: Double): Unit =
    println("charging " + price)

class Coffee:
  val price: Double = 2.0

case class Charge(cc: CreditCard, amount: Double):
  def combine(other: Charge): Charge =
    if cc == other.cc then Charge(cc, amount + other.amount)
    else throw Exception("Can't combine charges with different cards")

class Cafe:
  def buyCoffee(cc: CreditCard): (Coffee, Charge) =
    val cup = Coffee()
    (cup, Charge(cc, cup.price))

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) =
    val purchases: List[(Coffee, Charge)] =
      List.fill(n)(buyCoffee(cc))

    val (coffees, charges) = purchases.unzip
    val reduced =
      charges.reduce((c1, c2) => c1.combine(c2))
    (coffees, reduced)

def maybeTwice(b: Boolean, i: => Int) = if b then i + i else 0
val x = maybeTwice(true, { println("hi"); 1 + 41 })
