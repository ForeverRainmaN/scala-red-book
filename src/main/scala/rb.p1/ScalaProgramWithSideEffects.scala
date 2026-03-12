object ScalaProgramWithSideEffects extends App {

  class Cafe {
    def buyCoffee(cc: CreditCard): Coffee = {
      val cup = new Coffee()
      cc.charge(cup.price)
      cup
    }
  }

  class CreditCard {
    def charge(price: Double): Unit = {
      println("charging " + price)
    }
  }

  class Coffee {
    val price: Double = 2.0
    val cc            = new CreditCard()
    val cafe          = new Cafe()
    val cup           = cafe.buyCoffee(cc)
  }

  class Charge(val cc: CreditCard, val amount: Double) {
    def combine(other: Charge): Charge = {
      require(cc == other.cc, "Нельзя объединить платежи с разных карт")
      new Charge(cc, amount + other.amount)
    }
  }
}

object Part1Exercises extends App {
  /*
    Write	a	recursive	function	to	get	the	nth	Fibonacci	number
(http://mng.bz/C29s).	The	first	two	Fibonacci	numbers	are	0	and	1.	The
nth	number	is	always	the	sum	of	the	previous	two—the	sequence	begins
0,	1,	1,	2,	3,	5.	Your	definition	should	use	a	local,	tail-recursive

   */

  def fib(n: Int): Int = {
    if (n == 0) n
    else if (n < 2) 1
    else fib(n - 1) + fib(n - 2)
  }

  println(fib(10)) // 55

  def fib2(n: Int): Int = {
    @annotation.tailrec
    def go(i: Int, current: Int, previous: Int): Int = {
      if (i <= 0) current
      else {
        val newCurr    = current + previous
        val newPrevius = current

        println("new CURRENT: " + newCurr)
        println("new PREVIOUS: " + newPrevius)

        go(i - 1, newCurr, newPrevius)
      }
    }

    go(n, n - 1, n - 2)
  }
  println(fib2(10))
  /*
    i = 10
    current = 9
    previous = 8

    10 = 9 + 8
    9 = 8 +z


    i = 9
    current = 17
    previous = 8

    i = 8

    current = 25
    previous = 8

    i = 7
    current = 33
    previous = 8


    i = 6
    current = 41
    previous = 8

    i = 6
    current = 49
    previous = 8

    i = 6
    current = 47
    previous = 8

    i = 5

   */

}
