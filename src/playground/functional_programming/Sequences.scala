package playground.functional_programming

import java.lang

object Sequences extends App {

  //Seq
  val arange: Seq[Int] = 1 until 10
  arange.foreach(println)

//  (1 to 10).foreach(x => print("@"))

  trait Animal
  case class Dog(name: String) extends Animal
  case class Cat(name: String) extends Animal

  val pet: Seq[Animal] = Seq(Dog("techies"), Cat("specter"))
  pet.foreach(println)

  // List
  val aList = List.fill(5)(1)
  val appended = 0 +: aList :+ 5
  println(appended.mkString("---"))

  //array
  val n = Array(1, 2, 3, 4)
  n.update(2, 0)
  println(n.mkString(" - "))

  val maxTime = 1000
  val max_capacity = 1000000
  val r = scala.util.Random
  def perform(collection: Seq[Int]): Double = {
    val averageTime = for (i <- 1 to maxTime) yield {
      val currTime = System.nanoTime()
      collection.updated(r.nextInt(1000000), r.nextInt(1000000))
      System.nanoTime() - currTime
    }
    averageTime.sum * 1.0 / maxTime
  }
  val list_ = (1 to max_capacity).toList
  val vector_ = (1 to max_capacity).toVector
  println("List perform:... ", perform(list_))
  println("Vector perform:...", perform(vector_))
  println("list time / vector time", perform(list_) / perform(vector_))

}
