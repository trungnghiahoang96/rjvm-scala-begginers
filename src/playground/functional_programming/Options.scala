package playground.functional_programming

import scala.util.Random

object Options extends App {
  println("Rock the JVM exercise:   ")

  /*
   Exercise.
   */

  class Connection {
    def connect = "Connected" // connect to some server
  }
  object Connection {
    val random = new Random(System.nanoTime())

    def apply(host: String, port: String): Option[Connection] =
      //random.nextBoolean()
      if (true) Some(new Connection)
      else None
  }
  val config: Map[String, String] = Map(
    // fetched from elsewhere
    "host" -> "176.45.36.1",
    "port" -> "80"
  )
  // try to establish a connection, if so - print the connect method
  val host = config.get("host")
  val port = config.get("port")
  val connection: Option[Connection] = for {
    h <- host
    p <- port
    connectTry <- Connection(h, p)
  } yield connectTry
  val connectionStatus = connection.map(c => c.connect)
  println(connectionStatus)
  println(connectionStatus.foreach(println))

  config
    .get("host")
    .flatMap(
      h =>
        config
          .get("port")
          .flatMap(p => Connection(h, p))
    )
    .map(c => c.connect)
    .foreach(println)

  println("*" * 100)
  println(
    "Practice follow: https://freecontent.manning.com/using-option-in-scala-part-1-introduction/",
    "*" * 50
  )

//quick check 1
  def filter(text: String, word: String): String = {
    def filterHelper(text: String, word: String): Option[String] = {
      if (text.contains(word)) Some(text) else None
    }
    filterHelper(text, word) match {
      case Some(result) => result
      case None         => "Not Found"
    }
  }
  println(filter("Data Engineer", "Data"))

  //quick check 2
  def greetings(mess: Option[String]): String = {
    mess match {
      case Some(result) => s"Hello $result!"
      case None         => "Hello Human!"
    }
  }
  println(greetings(Some("Scala")))
  println("*" * 50)
  println("Map Flatten flatMap.....")

  case class Person(name: String, age: Int, drivingLicense: Option[String])
  case class Car(model: String,
                 owner: Option[Person],
                 registerPlate: Option[String])
  def ownerCar(car: Car): Option[String] = {
//    car.owner match {
//      case None         => "brand new!"
//      case Some(result) => result.name
//
//    }
    //map function handle None for us
    car.owner.map(_.name)
  }
  def extractRegistrationPlate(car: Car): Option[String] = {
    car.registerPlate.map(_.toUpperCase)
  }

  // even use Option[Car] instead of Car
  def extractDrivingLicense(optCar: Option[Car]): Option[String] = {
    // use case for car: Car
//    car.owner.flatMap(_.drivingLicense)

    //use case for car: Option[Car]
    //start flatMap right after we encounter first Option value
    optCar.flatMap { car =>
      {
        car.owner.flatMap { person =>
          person.drivingLicense
        }
      }
    }
  }
  def extractDLFor(optCar: Option[Car]): Option[String] =
    for {
      car <- optCar
      owner <- car.owner
      drivingLicense <- owner.drivingLicense
    } yield drivingLicense

  def ownerBelowAge(car: Car, age: Int): Option[String] = {
    //my try
//    if (car.owner.map(_.age).get < age) car.owner.map(_.name)
//    else None

    //author
    car.owner.flatMap { person =>
      {
        if (person.age < age) Some(person.name)
        else None
      }
    }
  }
  def ownerBelowAgeFor(optCar: Option[Car], age: Int): Option[String] =
    for {
      car <- optCar
      owner <- car.owner
      if owner.age < age
      returnName <- Some(owner.name)

    } yield returnName

  def carWithLicensedOwner(optCar: Option[Car]): Option[Car] =
//  {
//    // my try
//    optCar.flatMap { car =>
//      car.owner.flatMap { owner => {
//        if (owner.drivingLicense != None) optCar
//        else None
//      }
//      }
//    }
//  }
    //author
    optCar.find(car => car.owner.flatMap(_.drivingLicense).isDefined)

  val batman: Person =
    Person("Professional Data Engineer", 23, drivingLicense = Some("861962"))
  val myCar: Car =
    Car("Tesla Model 3", owner = Some(batman), registerPlate = Some("done"))

  println(ownerCar(myCar))
  println(extractRegistrationPlate(myCar))

  println(extractDrivingLicense(Some(myCar)))

  println(ownerBelowAge(myCar, 12))

  println("*" * 100)
  println("Option with for comprehension")
  println("extract DL: ", extractDLFor(Some(myCar)))
  println(ownerBelowAgeFor(Some(myCar), 12))
  println("get Car if onwner have DL: ", carWithLicensedOwner(Some(myCar)))
}
