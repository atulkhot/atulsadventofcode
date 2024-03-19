package example.day4part1

import cats.kernel.Monoid
import cats.implicits._

object MyTry1 {

  def main(args: Array[String]): Unit = {
    val xRows = (0 to 5).toList
    val yRows = (0 to 5).toList

    println(xRows.flatMap(i => yRows.map(j => (i, j))))
  }
}
