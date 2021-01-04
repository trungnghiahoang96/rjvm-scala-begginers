package playground.functional_programming

import java.nio.channels.NetworkChannel

object TuplesMaps extends App {

  val aTuple = ("Spark", 1)
  println(aTuple.getClass)
  println(aTuple.toString())
  println(aTuple.copy(_1 = "Airflow"))
  println(aTuple.swap)

  println("Map", "*" * 70)

  val stack =
    List("Scala", "Spark", "Airflow", "Akka", "Cats", "Java", "Python", "JVM")
  println(stack.groupBy(name => name.charAt(0)))

  val user = List("Data Engineer", "Python", "Data Scientist")
  val tech = List("Apache Spark", "PySpark", "SQL")
  println("Mapping tech:  ", (user zip tech).toMap)
  println((user zip tech).toMap.filterKeys(_.contains("Data")))

  var emptyMap = Map[String, String]()
  for (pair <- (user zip tech)) {
    emptyMap += pair
  }
  println("From empty to:  ", emptyMap)
  /*
   1.  What would happen if I had two original entries "Jim" -> 555 and "JIM" -> 900

       !!! careful with mapping keys.

   2.  Overly simplified social network based on maps
       Person = String
       - add a person to the network
       - remove
       - friend (mutual)
       - unfriend

       - number of friends of a person
       - person with most friends
       - how many people have NO friends
       - if there is a social connection between two people (direct or not)
   */
  val sameLowerName: Map[String, Int] = Map(("Scala", 5), "scalA" -> 1)
  println(sameLowerName.map(pair => pair._1.toLowerCase() -> pair._2))
  println("*" * 100)
  println("Social Network")

  def add(network: Map[String, Set[String]],
          person: String): Map[String, Set[String]] =
    network + (person -> Set())

  def friend(network: Map[String, Set[String]], a: String, b: String) = {
    val personA = network(a)
    val personB = network(b)
    network + (a -> (personA + b)) + (b -> (personB + a))
  }

  def unfriend(network: Map[String, Set[String]], a: String, b: String) = {
    val personA = network(a)
    val personB = network(b)
    network + (a -> (personA - b)) + (b -> (personB - a))
  }
  def remove(network: Map[String, Set[String]], person: String) = {
    def removeHelper(
      friends: Set[String],
      networkAcc: Map[String, Set[String]]
    ): Map[String, Set[String]] =
      if (friends.isEmpty) networkAcc
      else removeHelper(friends.tail, unfriend(network, person, friends.head))

    val unfriended = removeHelper(network(person), network)
    unfriended - person

  }
  val empty: Map[String, Set[String]] = Map()
  val network = add(add(add(empty, "Hoang"), "Trung"), "Nghia")

  println(network)
//  println(friend(network, "Hoang", "Nghia"))
//  println(unfriend(friend(network, "Hoang", "Nghia"), "Hoang", "Nghia"))
//  println(remove(friend(network, "Hoang", "Nghia"), "Nghia"))

  val nghiahoang = friend(network, "Hoang", "Nghia")
  val trungnghia = friend(nghiahoang, "Trung", "Nghia")
  val testNet = add(trungnghia, "Data Engineer")
  println(testNet)

  def nFriends(network: Map[String, Set[String]], person: String): Int =
    if (!network.contains(person)) 0
    else network(person).size
  def personMostFriends(network: Map[String, Set[String]]): String = {
//    network.maxBy(pair => pair._2.size)._1
    network.maxBy(_._2.size)._1
  }

  def peopleNoFriend(network: Map[String, Set[String]]): Iterable[String] =
//    network
//      .map(pair => (pair._1, pair._2.size))
//      .filter(pair => pair._2 == 0)
//      .size
    network.filterKeys(network(_).isEmpty).keys

  //advanced using BFS
  def checkConnect(network: Map[String, Set[String]],
                   a: String,
                   b: String): Boolean = {
    network(a).contains(b)

  }

  println("# friend of Nghia: ", nFriends(testNet, "Nghia"))
  println("Peple most friend: ", personMostFriends(testNet))
  println("# people no friend ", peopleNoFriend(testNet))
  print(checkConnect(network, "Nghia", "DataEngineer"))

}
