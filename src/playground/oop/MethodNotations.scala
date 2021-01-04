package playground.oop

object MethodNotations extends App {

  class Person(val name: String, nicePP: String, val age: Int) {
    def likes(pp: String): Boolean = pp == this.nicePP
    // infix methods
    def +(person: Person): String =
      s"${name} and ${person.name} is doing pair programming"

    def +(nickname: String): Person =
      new Person(s"${name} - the ${nickname}", nicePP, age)

    //prefix methods
    def unary_! : String = s"${name} is a bad software engineer"
    def unary_+ : Person = new Person(name, nicePP, age + 1)

    //postfix methods
    def isAlive: Boolean = true

    def learns(tech: String): String = s"$name learns $tech"
    def learnsScala: String = this learns "Scala - Big Data"

    //apply
    def apply(): String = s"${name} is a Data Engineer"
    def apply(n: Int): String =
      s"${name} is a Data Engineer with $n years of experience"

  }
  val nghia = new Person("Nghia", "(Functional, Probabilistic) Programming", 22)
  println(nghia.likes("Object-oriented programming"))
  val hieu = new Person("hieu", "functional", 22)

  //infix method
  println("play infix method")
  println(nghia + hieu) // nghia.+(hieu)
  println((nghia + "Pragmatic Dreamer  (Bayesian)")())
  println("\n")

  println("play with prefix method")
  println(!nghia) // nghia.unary_!
  println((+nghia).age)
//  println(nghia.unary_+.age) // equivalent to what expression of prefix ??
  println("\n")

  println("play with postfix method")
  println("IS Nghia alive", nghia isAlive) // nghia.isAlive
  println(nghia.learns("Airflow"))
  println(nghia learnsScala)

  println("\n")

  println("apply method for calling instance as function")
  println(nghia()) // equivalent  nghia.apply()
  println(nghia(2))
}
