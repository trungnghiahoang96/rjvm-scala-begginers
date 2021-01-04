package playground

import java.awt.font.TransformAttribute

// MyList implement start at 17th video
// in 18 video add Generic

// 20th video expand MyList by adding traits and methods:

//1.  Generic trait MyPredicate[-T] with a little method test(T) => Boolean
//2.  Generic trait MyTransformer[-A, B] with a method transform(A) => B
//3.  MyList:
//- map(transformer) => MyList
//- filter(predicate) => MyList
//- flatMap(transformer from A to MyList[B]) => MyList[B]
//
//class EvenPredicate extends MyPredicate[Int]
//class StringToIntTransformer extends MyTransformer[String, Int]
//
//[1,2,3].map(n * 2) = [2,4,6]
//[1,2,3,4].filter(n % 2) = [2,4]
//[1,2,3].flatMap(n => [n, n+1]) => [1,2,2,3,3,4]
//*/

abstract class MyList17[+A] {

  /*
     head = first element of  the  list
     tail = remainder of the list
     isEmpty = is this list empty
     add(int) => new list with this element added
     toString => a string representation of the list
   */

  def head: A
  def tail: MyList17[A]
  def isEmpty: Boolean
  // add B
  def add[B >: A](element: B): MyList17[B]
  def printElements: String
  // polymorphic call
  override def toString: String = "[" + printElements + "]"

  def map[B](transformer: A => B): MyList17[B]
  def filter(predicate: A => Boolean): MyList17[A]

  // essential for flatMap implementation
  def ++[B >: A](list: MyList17[B]): MyList17[B]
  def flatMap[B](transformer: A => MyList17[B]): MyList17[B]
// HOFs
  def foreach(f: A => Unit): Unit
  def sort(compare: (A, A) => Int): MyList17[A]
  def zipWith[B, C](list: MyList17[B], zip: (A, B) => C): MyList17[C]
  def fold[B](start: B)(operator: (B, A) => B): B

}
// for creating Empty list of String/ Int (can add String/ Int - [B] after that
// so A is Nothing type
case object Empty extends MyList17[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList17[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing](element: B): MyList17[B] = new Cons(element, Empty)
  def printElements: String = ""

  def map[B](transformer: Nothing => B): MyList17[B] = Empty
  def filter(predicate: Nothing => Boolean): MyList17[Nothing] = Empty

  def ++[B >: Nothing](list: MyList17[B]): MyList17[B] = list
  def flatMap[B](transformer: Nothing => MyList17[B]): MyList17[B] = Empty

  //HOFs
  def foreach(f: Nothing => Unit): Unit = ()
  def sort(compare: (Nothing, Nothing) => Int): MyList17[Nothing] = Empty
  def zipWith[B, C](list: MyList17[B], zip: (Nothing, B) => C): MyList17[C] = {
    if (!list.isEmpty)
      throw new RuntimeException("Lists do not have same length")
    else Empty
  }

  def fold[B](start: B)(operator: (B, Nothing) => B): B = start

}

case class Cons[+A](h: A, t: MyList17[A]) extends MyList17[A] {
  def head: A = h
  def tail: MyList17[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B) = new Cons(element, this)
  def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  def map[B](transformer: A => B): MyList17[B] =
    new Cons(transformer(h), t.map(transformer))

  def filter(predicate: A => Boolean): MyList17[A] =
    if (predicate(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)

  def ++[B >: A](list: MyList17[B]): MyList17[B] = new Cons(h, t ++ list)
  def flatMap[B](transformer: A => MyList17[B]): MyList17[B] =
    transformer(h) ++ t.flatMap(transformer)

  //HOFs
  def foreach(f: A => Unit): Unit = {
    f(h)
    t.foreach(f)
  }
  def sort(compare: (A, A) => Int): MyList17[A] = {
    def insert(x: A, sortedList: MyList17[A]): MyList17[A] = {
      if (sortedList.isEmpty) new Cons(x, Empty)
      else if (compare(x, sortedList.head) <= 0) new Cons(x, sortedList)
      else new Cons(sortedList.head, insert(x, sortedList.tail))
    }
    val sortedTail = t.sort(compare)
    insert(h, sortedTail)
  }

  def zipWith[B, C](list: MyList17[B], zip: (A, B) => C): MyList17[C] = {
    if (list.isEmpty)
      throw new RuntimeException("Lists do not have same length")
    else new Cons(zip(h, list.head), t.zipWith(list.tail, zip))
  }
  /*
  [1,2,3].fold(0)(+) =
  [2,3].fold((0 + 1))(+) =
  [3].fold(((0+1) + 2 ))(+) =
  operator(0 + 1 + 2 + 3)

   */
  def fold[B](start: B)(operator: (B, A) => B): B = {
    if (t.isEmpty) operator(start, h)
    else t.fold(operator(start, h))(operator)
    // or only t.fold(operator(start, h))(operator)
  }
}

//trait MyPredicate[-T] {
//  def test(elem: T): Boolean
//}
//// a kind of behavior
//trait MyTransformer[-A, B] {
//  def transformer(elem: A): B
//}

object ListTest extends App {
  val listOfInteger: MyList17[Int] =
    new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Cons(5, Empty)))))
  val listOfString: MyList17[String] =
    new Cons("Scala", new Cons("Apache Spark", Empty))
//  println(listOfInteger.head)
//  println(listOfInteger.tail.head)
//  println(listOfInteger.toString)
  println(listOfInteger)
  println(listOfString)

  print("Map (lambda x: x*5) work with listOfInteger using Anonymous Class: ")
  // wtf why "new" here
  println(
    listOfInteger
      .map(_ * 5)
      .toString
  )

  print(
    "Filter (lambda x: x%2 == 1) work with listOfInteger using Anonymous Class: "
  )
  println(
    listOfInteger
      .filter(_ % 2 == 1)
      .toString
  )
  print("flatMap tranform (lambda n:Int =  [n, n*3]) for listOfInteger: ")
  println(
    listOfInteger
      .flatMap(elem => new Cons(elem, new Cons(elem * 3, Empty)))
      .toString
  )

  println("*" * 100)
  println("Higher order functions")
  listOfInteger.foreach(println)

  //zipWith
  println("zipWith...........")
  val listOfInteger2: MyList17[Int] = {
    new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Cons(5, Empty)))))
  }

  val simpleMul: (Int, Int) => Int = _ * _
  listOfInteger.zipWith(listOfInteger2, simpleMul).foreach(println)

  println("my implement of def fold....")
  println(listOfInteger.fold(1)((a: Int, b: Int) => a + b))

//  //play Dog Cat Animal Covariance (self practice by 18th video)
//  abstract class Animal {
//    def name: String
//    override def toString: String = s"$name"
//  }
//  case class Cat(name: String) extends Animal
//  case class Dog(name: String) extends Animal
//  class Car(name: String) {
//    override def toString: String = s"$name"
//  }
//  val cat1: Cat = new Cat("Cat1")
//  val listOfCat: MyList17[Cat] =
//    new Cons(Cat("Cat1"), new Cons(Cat("Cat2"), Empty))
//  println("List of Cat", listOfCat)
//
//  val mixedListCatDog: MyList17[Cat] =
//    new Cons(Cat("Cat1"), new Cons(Cat("Cat2"), new Cons(Cat("Cat3"), Empty)))
//
//  //start out with Only Cat type but thanks to Covariant,
//  // it can use method add  for Dog as well
//  println("Mix Cat Dog List: ", mixedListCatDog.add(Dog("Dog1")))
//  // end self practice 18th video

}
