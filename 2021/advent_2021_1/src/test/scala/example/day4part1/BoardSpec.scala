package example.day4part1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import cats._
import cats.data.State
import cats.data._
import cats.implicits._
import org.scalatest.OptionValues.convertOptionToValuable

class BoardSpec extends AnyFlatSpec with should.Matchers {
  "A board representation" should "return proper coordinates" in {
    val list = List("14", "21", "17", "24", "4", "10", "16", "15", "9", "19", "18", "8", "23",
      "26", "20", "22", "11", "13", "6", "5", "2", "0", "12", "3", "7").map(_.toInt)

    val board = Board(5, 5, list)

    board.board(14) should be (List(0 -> 0))
    board.board(11) should be (List(3 -> 1))
    board.board(3) should be (List(4 -> 3))
  }

  "A board representation with duplicate numbers" should "return proper coordinates" in {
    val list = List("14", "21", "13", "24", "4", "13", "16", "15", "9", "19", "18", "8", "23",
      "26", "20", "22", "11", "13", "6", "5", "2", "0", "12", "3", "4").map(_.toInt)

    val board = Board(5, 5, list)

    board.board(4) should be (List(0 -> 4, 4 -> 4))
    board.board(13) should be (List(0 -> 2, 1 -> 0, 3 -> 2))
  }

  "Marking an element" should "update the coordinates vectors" in {
    val list = List("14", "21", "13", "24", "4", "13", "16", "15", "9", "19", "18", "8", "23",
      "26", "20", "22", "11", "13", "6", "5", "2", "0", "12", "3", "4").map(_.toInt)

    val boardState = Board(5, 5, list)
    val initialSize = boardState.board.size

    val startingState: State[Int, Board] = boardState.markACoordinate

    val allMarkedState = for {
      initState <- startingState
      finalState <- initState.markACoordinate
    } yield finalState

    val resultState: Board = startingState.runA(14).value

    resultState.rows.get(0).value should be (1)
    resultState.cols.get(0).value should be (1)

    val q = allMarkedState.runA(14).value

    q.board.size should be (initialSize-1)
  }

  "Marking an element so a single row" should "is completely marked" in {
    val list = List("14", "14", "14", "14", "14", "13", "16", "15", "9", "19", "18", "8", "23",
      "26", "20", "22", "11", "13", "6", "5", "2", "0", "12", "3", "4").map(_.toInt)

    val boardState = Board(5, 5, list)

    val startingState: State[Int, Board] = boardState.markACoordinate

    val resultState: Board = startingState.runA(14).value

    resultState.rows.get(0).value should be (1)
  }

}