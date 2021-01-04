package playground.functional_programming

import scala.annotation.tailrec

object HOFCurriesPlayground extends App {

  // function that applies a function n times over a value x
  // nTimes(f, n, x)
  // nTimes(f, 3, x) = f(f(f(x))) = nTimes(f, 2, f(x)) = f(f(f(x)))
  // nTimes(f, n, x) = f(f(...f(x))) = nTimes(f, n-1, f(x))

  //my try implementation
//  def nTimes_(f: Int => Int, n: Int, x: Int): Int = {
//    @tailrec
//    def helper(f: Int => Int, x: Int, n: Int, acc: Int): Int =
//      if (n < 1) acc
//      else helper(f, n - 1, x, f(acc))
//
//    helper(f, n, x, f(x))
//  }

  // ntb(f,n) = x => f(f(f...(x)))
  // increment10 = ntb(plusOne, 10) = x => plusOne(plusOne....(x))
  // val y = increment10(1)
  //return a function

  def nTimesBetter_(f: Int => Int, n: Int): Int => Int = {
    if (n < 1) (x: Int) => x
    else (x: Int) => nTimesBetter_(f, n - 1)(f(x))
  }

  //lean implementation
  def nTimes_(f: Int => Int, n: Int, x: Int): Int = {
    if (n < 1) x
    else nTimes_(f, n - 1, f(x))

  }
  //  val plusOne_ = (x: Int) => x + 1
  //  println(nTimes_(plusOne_, 10, 1))
  //  val plusOne10 = nTimesBetter_(plusOne_, 10)
  //  println(plusOne10(1))

  //EXERCISE HOF

  //toCurry(f: (Int, Int) => Int) => (Int => Int => Int)
  def curriedFormatter(c: String)(x: Double): String = c.format(x)

  val standardFormat: (Double => String) = curriedFormatter("%4.2f")
  val preciseFormat: (Double => String) = curriedFormatter("%10.8f")

//  println(standardFormat(Math.PI))
//  println(preciseFormat(Math.PI))
  println("%10.8f".format(Math.PI))

//  2.  toCurry(f: (Int, Int) => Int) => (Int => Int => Int)
//  fromCurry(f: (Int => Int => Int)) => (Int, Int) => Int
//
//  3.  compose(f,g) => x => f(g(x))
//  andThen(f,g) => x => g(f(x))

  val simpleAdd: (Int, Int) => Int = _ + _
  val simpleExponential: (Int => Int) = (x: Int) => x ^ 2
  val simpleMul: (Int => Int) = (x: Int) => x * 3

  println("*" * 100)
  def toCurry(f: (Int, Int) => Int): (Int => Int => Int) = {
    (x: Int) => (y: Int) =>
      f(x, y)
  }

  val currySimpleAdd = toCurry(simpleAdd)
  println("tranform simpleAdd to curry   ", currySimpleAdd(3)(4))

  def fromCurry(f: (Int => Int => Int)): (Int, Int) => Int = {
    (x: Int, y: Int) =>
      f(x)(y)
  }
  val backToSimpleAdd = fromCurry(currySimpleAdd)
  println("currySimpledAdd back to simpleAdd ", backToSimpleAdd(4, 5))

  println("*" * 100)
  println("combine function.......")
  def compose[A, B, C](f: (B => C), g: (A => B)): (A => C) = { x =>
    f(g(x))
  }
  def andThen[A, B, C](f: (B => C), g: (C => A)): (B => A) = { x =>
    g(f(x))
  }

}
