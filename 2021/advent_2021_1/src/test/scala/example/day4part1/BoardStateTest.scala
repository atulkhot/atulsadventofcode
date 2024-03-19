package example.day4part1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BoardStateTest extends AnyFlatSpec with should.Matchers {
  "A board representation" should "return proper coordinates" in {
    val list = List("14", "21", "17", "24", "4", "10", "16", "15", "9", "19", "18", "8", "23",
      "26", "20", "22", "11", "13", "6", "5", "2", "0", "12", "3", "7").map(_.toInt)

    val board = BoardState(5, 5, list)

    board.board(14) should be (List(0 -> 0))
    board.board(11) should be (List(3 -> 1))
    board.board(3) should be (List(4 -> 3))
  }

  "A board representation with duplicate numbers" should "return proper coordinates" in {
    val list = List("14", "21", "13", "24", "4", "13", "16", "15", "9", "19", "18", "8", "23",
      "26", "20", "22", "11", "13", "6", "5", "2", "0", "12", "3", "4").map(_.toInt)

    val board = BoardState(5, 5, list)

    board.board(4) should be (List(0 -> 4, 4 -> 4))
    board.board(13) should be (List(0 -> 2, 1 -> 0, 3 -> 2))
  }

}
