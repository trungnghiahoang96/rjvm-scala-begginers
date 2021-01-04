package playground

object Functions extends App {

  def introUrSelf(name: String = "nghia", job: String, exp: Int): String = {
    s"Hello, My name is $name. I have been working as $job for $exp years."
  }
  println(introUrSelf("trung nghia", "Data Engineer", 3))

  def simpleRecursion(n: Int, aPhrase: String): String = {
    if (n == 1) aPhrase
    else aPhrase + simpleRecursion(n - 1, aPhrase)
  }
  println("simple Recursion", simpleRecursion(5, "Scala"))

  def aPlayFactorial(n: Int): Int = {
    if (n <= 0) 1
    else n * aPlayFactorial(n - 1)
  }
  println("a play with Factorial recursive", aPlayFactorial(5))

  def aFibonacci(n: Int): Int = {
    if (n <= 2) 1
    else aFibonacci(n - 1) + aFibonacci(n - 2)
  }

  println(
    "a play with Fibonacci with recursive style of Scala and accidently solve!! Nghia ",
    aFibonacci(8)
  )

  //Checking number is Prime
  def aPrimeCheck(n: Int): Boolean = {

    def PrimeUntil(t: Int): Boolean =
      if (t <= 1) true
      else n % t != 0 && PrimeUntil(t - 1)

    if (n <= 1) false
    else PrimeUntil(n / 2)
  }

  for (i <- 1 to 10 if aPrimeCheck(i))
    println(s"$i is a Prime Number", i)

  println("a list of Prime from 1 to 30:")
  val filteredPrime = for (i <- 1 to 30 if aPrimeCheck(i)) yield i
  filteredPrime.foreach(println)

}
