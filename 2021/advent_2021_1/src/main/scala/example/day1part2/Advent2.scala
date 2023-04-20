package example.day1part2

import scala.io.Source

object Advent2 {

  private val dataFile = "./src/main/resources/example/day1part2/example_data.txt"

  val depthsList =
    Source.fromFile(dataFile)
      .getLines()
      .map(_.toInt)
      .toList

  def main(args: Array[String]): Unit = {
    val sums = depthsList.sliding(3).map(_.sum)
    val pairs = sums.sliding(2).map(arr => (arr(0), arr(1)))
    val result = pairs.count {
      case (prev, next) => prev < next
    }
    println(result)
  }
}