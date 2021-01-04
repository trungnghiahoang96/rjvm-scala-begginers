package playground.patternMatching

object patternMatching extends App {
  val x: Any = "Scala"
  val aVar = x match {
    case something => s"i find $something"
  }
  val aTuple = (1, 2)
  val matchTup = aTuple match {
    case (2, b) => s"find a tuple (2, $b)"
    case (a, b) => s"($a, $b)"
  }

  println(aVar)
  println(matchTup)

}
