package example.day4part1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


class Advent4Test extends AnyFlatSpec with should.Matchers {
  "A list transpose" should "first row elements as first column elements" in {
    val list = List("14", "21", "17", "24", "4", "10", "16", "15", "9", "19", "18", "8", "23",
      "26", "20", "22", "11", "13", "6", "5", "2", "0", "12", "3", "7").map(_.toInt)

    val transposedList: List[Int] = Advent4.transpose(list)
    transposedList.isEmpty should be (false)

    transposedList(0) should be (14)
    transposedList(1) should be (10)
    transposedList(2) should be (18)
    transposedList(3) should be (22)
    transposedList(4) should be (2)
  }

}
