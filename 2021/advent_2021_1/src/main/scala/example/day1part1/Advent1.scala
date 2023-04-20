package example.day1part1

import scala.io.Source

object Advent1 {

  private val dataFile = "./src/main/resources/example/day1part1/example_data.txt"
  val depthsList =
    Source.fromFile(dataFile)
      .getLines()
      .map(_.toInt)
      .toList

  def main(args: Array[String]): Unit = {
    val pairs = depthsList.sliding(2).map(arr => (arr(0), arr(1))) // depthsList.zip(depthsList.tail)
    val result = pairs.count {
      case (first, second) => first < second
    }
    println(result)
  }
}