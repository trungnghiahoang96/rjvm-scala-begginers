package playground.functional_programming

object MapFlatmapFilterFor extends App {

  val scale = List(1, 2, 3)
  val tech = List("Spark", "Airflow", "AWS")
  val level = List("Junior", "Senior", "Achitect")
  val combi =
    tech.flatMap(t => level.flatMap(l => scale.map(s => t + " " + l + " " + s)))
  /* prefer for readable
  val forCombinations = for {
    n <- numbers if n % 2 == 0
    c <- chars
    color <- colors
  } yield "" + c + n + "-" + color
  println(forCombinations)
   */
  println(combi)
}
