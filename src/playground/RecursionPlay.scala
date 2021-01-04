package playground
import scala.annotation.tailrec

object RecursionPlay extends App {

  def aFactorial(n: Int): Int = {
    println(
      "Need to compute factorial of " + n + "  -  Need to compute factorial of " + (n - 1)
    )
    if (n <= 1) 1
    else {
      val tmp_factorial = n * aFactorial(n - 1)
      println("Computed Factorial of " + n + " : " + tmp_factorial)
      tmp_factorial
    }
  }

  def tailRecFactorial(n: Int): BigInt = {
    def factHelper(x: Int, acc: Int): BigInt = {
      if (x <= 1) acc
      else factHelper(x - 1, acc * x)
    }

    factHelper(n, 1)

  }

  val num: Int = 8
  println("Compute factorial of " + num, aFactorial(num))
  println(" \n")
  println("Factorial with tail recursion of " + num, tailRecFactorial(8))
  println("\n")

  //1. concatenate a string n times
  @tailrec
  def tailRecConcat(s: String, n: Int, accumulator: String = ""): String = {
    if (n < 1) accumulator
    else tailRecConcat(s: String, n - 1, accumulator + s)

  }

  println(
    "tail recursion for repetitive string : ",
    tailRecConcat("Airflow__", 5)
  )
  println("\n")

  //2. isPrime implement with Tail Recursion

  def tailRecIsPrime(n: Int): Boolean = {
    @tailrec
    def isPrimeHelper(x: Int, accumulator: Boolean = true): Boolean = {
      if (x <= 1) accumulator
      else isPrimeHelper(x - 1, accumulator && (n % x != 0))
    }

    if (n < 2) false
    else isPrimeHelper(n / 2)

  }

  print("tail recursion for checking Prime from 1 to 20: ")
  val filterPrimes = for (i <- 1 to 20 if tailRecIsPrime(i)) yield i
  filterPrimes.foreach(x => print(x + " "))
  print("\n")

  //3. tail recursion for fibonacci
  def tailRecFibonacci(n: Int): Int = {
    def FibHelper(i: Int = 2, last: Int, nextToLast: Int): Int = {
      if (i >= n) last
      else FibHelper(i + 1, last + nextToLast, last)
    }
    FibHelper(last = 1, nextToLast = 1)
  }
  print("tail recursion Fibonacci", tailRecFibonacci(8))
}
