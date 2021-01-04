package playground.oop

object OOBasics extends App {

  class Writer(name: String, surname: String, var yob: Int) {

    def fullname(): String = this.surname + this.name
  }
  val nghia = new Writer("trung nghia", "hoang ", 1996)
  println(nghia.fullname())

  class Novel(name: String, releaseYear: Int, author: Writer) {
    def ageAtRelease: Int = (this.releaseYear - author.yob)
    def isWrittenBy(author: Writer): Boolean = author == this.author
    def copy(newYear: Int): Novel = new Novel(name, newYear, author)
  }

  val myBook = new Novel("how i become Data Engineer", 2020, author = nghia)
  println(myBook.ageAtRelease)
  println(myBook.isWrittenBy(nghia))

  val trung = new Writer("trung nghia", "hoang ", 1996)
  println("checking 2nd edition with author Trung")
  println(myBook.copy(2022).ageAtRelease)
  println(myBook.copy(2021).isWrittenBy(trung))

  // try counter implement
  class Counter(val count: Int = 0) {
    def inc: Counter = {
      println("Incrementing: ")
      new Counter(count + 1)
    }
    def dec: Counter = {
      println("Decrementing: ")
      new Counter(count - 1)
    }

    def inc(n: Int): Counter = {
      if (n <= 0) this
      else inc.inc(n - 1)
    }

    def dec(n: Int): Counter = {
      if (n <= 0) this
      else dec.dec(n - 1)
    }
    def print: Unit = println(count)
  }
  val cHour: Counter = new Counter(10)
  cHour.inc(5).print

}
