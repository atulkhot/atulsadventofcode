package example.day3part1

import scala.io.Source

object Advent_Day3 {
  private val dataFile = "./src/main/resources/example/day3part1/example_data.txt"
  //  private val dataFile = "./src/main/resources/example/day3part1/sample.txt"

  private val dataList =
    Source.fromFile(dataFile)
      .getLines()
      .toList

  def bitsAtPos(list: List[String], pos: Int): List[Char] =
    list.flatMap(s => List(s(pos)))

  private def computeBitsAtPos(list: List[String], pos: Int) = {
    bitsAtPos(list, pos).partition((x: Char) => x.toString == "1")
  }

  private def gammaBitAt(list: List[String], pos: Int): String = {
    val (ones, zeroes) = computeBitsAtPos(list, pos)
    if (ones.size > zeroes.size) "1" else "0"
  }

  private def epsilonBitAt(list: List[String], pos: Int): String = {
    val (ones, zeroes) = computeBitsAtPos(list, pos)
    if (ones.size > zeroes.size) "0" else "1"
  }

  def main(args: Array[String]): Unit = {

    val charsInBinaryNumber = dataList.headOption.map(_.length).getOrElse(0)

    val gammaStr = for {
      i <- (0 until charsInBinaryNumber)
      gammaBit <- gammaBitAt(dataList, i)
    } yield gammaBit

    val epsilonStr = gammaStr.map(c => if (c == '0') '1' else '0')

    val gammaNumber = Integer.parseInt(gammaStr.mkString, 2)
    val epsilonNumber = Integer.parseInt(epsilonStr.mkString, 2)

    val powerConsumption = gammaNumber * epsilonNumber
    println(powerConsumption)
  }
}
