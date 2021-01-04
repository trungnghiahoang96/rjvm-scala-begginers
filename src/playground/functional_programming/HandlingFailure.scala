package playground.functional_programming

import playground.functional_programming.Options.Connection

import scala.util.{Failure, Random, Success, Try}

object HandlingFailure extends App {
  val sthSuccess = Success(3)
  //map filter flatMap
  println(sthSuccess.map(_ * 2))
  println(sthSuccess.flatMap(x => Success(x * 10)))

  //for comprehension
  println("*" * 100)
  val host = "localhost"
  val port = "8080"

  def renderHTML(page: String) = println(page)

  class Connection {
    def get(url: String): String = {
      val random = new Random(System.nanoTime())
      if (random.nextBoolean()) "<html>...</html>"
      else throw new RuntimeException("Connection interrupted")
    }

    def safeGet(url: String): Try[String] = Try(get(url))
  }
  object HttpService {
    val random = new Random(System.nanoTime())

    def getConnection(host: String, port: String): Connection =
      if (random.nextBoolean()) new Connection
      else throw new RuntimeException("Someone else took the port")

    def safeGetConnection(host: String, port: String): Try[Connection] =
      Try(getConnection(host, port))
  }

  HttpService
    .safeGetConnection(host, port)
    .flatMap(c => c.safeGet("url"))
    .foreach(renderHTML)

  for {
    connection <- HttpService.safeGetConnection(host, port)
    maybeHTML <- connection.safeGet("url")
  } renderHTML(maybeHTML)
}
