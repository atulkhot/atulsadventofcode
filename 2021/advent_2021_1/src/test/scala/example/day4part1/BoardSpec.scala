package example.day4part1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import cats._
import cats.data.State
import cats.data._
import cats.implicits._
import example.day4part1.Board.BoardState
import org.scalatest.OptionValues.convertOptionToValuable

class BoardSpec extends AnyFlatSpec with should.Matchers {
  "A board representation" should "return proper coordinates" in {
    val list = List("14", "21", "17", "24", "4",
                    "10", "16", "15", "9", "19",
                    "18", "8", "23", "26", "20",
                    "22", "11", "13", "6", "5",
                    "2", "0", "12", "3", "7").map(_.toInt)

    val board = Board(5, 5, list)

    board.board(14) should be (List(0 -> 0))
    board.board(11) should be (List(3 -> 1))
    board.board(3) should be (List(4 -> 3))
  }

  "A board representation with duplicate numbers" should "return proper coordinates" in {
    val list = List("14", "21", "13", "24", "4",
                    "13", "16", "15", "9", "19",
                    "18", "8", "23", "26", "20",
                    "22", "11", "13", "6", "5",
                    "2", "0", "12", "3", "4").map(_.toInt)

    val board = Board(5, 5, list)

    board.board(4) should be (List(0 -> 4, 4 -> 4))
    board.board(13) should be (List(0 -> 2, 1 -> 0, 3 -> 2))
  }

  "Marking an element" should "update the coordinates vectors" in {
    val list = List("14", "21", "13", "24", "4",
                    "13", "16", "15", "9", "19",
                    "18", "8", "23", "26", "20",
                    "22", "11", "13", "6", "5",
                    "2", "0", "12", "3", "4").map(_.toInt)

    val board = Board(5, 5, list)
    val initialSize = board.board.size

    val startingState: BoardState[Unit] = Board.markBoardElement(14) // board.markACoordinate

    val resultState: Board = startingState.runS(board).value

    resultState.rows.get(0).value should be (1)
    resultState.cols.get(0).value should be (1)
    resultState.board.size should be (initialSize - 1)

    resultState.entireRowOrColMarked should be (false)
  }

  "Marking an element so a single row" should "is completely marked" in {
    val list = List("14", "14", "14", "14", "14",
                    "13", "16", "15", "9", "19",
                    "18", "8", "23", "26", "20",
                    "22", "11", "13", "6", "5",
                    "2", "0", "12", "3", "4").map(_.toInt)

    val boardState = Board(5, 5, list)
    val initialSize = boardState.board.size

    val startingState: BoardState[Unit] = Board.markBoardElement(14)

    val resultState: Board = startingState.runS(boardState).value

    resultState.rows.get(0).value should be (5)
    resultState.cols.get(0).value should be (1)
    resultState.cols.get(1).value should be (1)
    resultState.cols.get(2).value should be (1)
    resultState.cols.get(3).value should be (1)
    resultState.cols.get(4).value should be (1)
    resultState.board.size should be (initialSize - 1)

    resultState.entireRowOrColMarked should be (true)
  }

  "Marking an element so a single column" should "is completely marked" in {
    val list = List(
      "14", "21", "13", "24", "4",
      "14", "16", "15", "9", "19",
      "14", "8", "23", "26", "20",
      "14", "11", "13", "6", "5",
      "14", "0", "12", "3",  "4").map(_.toInt)

    val boardState = Board(5, 5, list)
    val initialSize = boardState.board.size

    val startingState: BoardState[Unit] = Board.markBoardElement(14)

    val resultState: Board = startingState.runS(boardState).value

    resultState.cols.get(0).value should be (5)
    resultState.rows.get(0).value should be (1)
    resultState.rows.get(1).value should be (1)
    resultState.rows.get(2).value should be (1)
    resultState.rows.get(3).value should be (1)
    resultState.rows.get(4).value should be (1)
    resultState.board.size should be (initialSize - 1)

    resultState.entireRowOrColMarked should be (true)
    resultState.winningScore should be (3528)
  }

  "Marking a series of elements so a single row" should "is completely marked" in {
    val list = List(
      "14", "21", "17", "24", "4",
      "10", "16", "15", "9", "19",
      "18", "8", "23", "26", "20",
      "22", "11", "13", "6", "5",
      "2", "0", "12", "3",  "7").map(_.toInt)

    val boardState = Board(5, 5, list)
    val initialSize = boardState.board.size

    val finalState: BoardState[Unit] = for {
      _ <- Board.markBoardElement(7)
      _ <- Board.markBoardElement(4)
      _ <- Board.markBoardElement(9)
      _ <- Board.markBoardElement(5)
      _ <- Board.markBoardElement(11)
      _ <- Board.markBoardElement(17)
      _ <- Board.markBoardElement(23)
      _ <- Board.markBoardElement(2)
      _ <- Board.markBoardElement(0)
      _ <- Board.markBoardElement(14)
      _ <- Board.markBoardElement(21)
      rowMarkedState <- Board.markBoardElement(24)
    } yield rowMarkedState

    val resultState = finalState.runS(boardState).value

    resultState.rows.get(0).value should be (5)
    resultState.rows.get(1).value should be (1)
    resultState.rows.get(2).value should be (1)
    resultState.rows.get(3).value should be (2)
    resultState.rows.get(4).value should be (3)

    resultState.cols.get(0).value should be (2)
    resultState.cols.get(1).value should be (3)
    resultState.cols.get(2).value should be (2)
    resultState.cols.get(3).value should be (2)
    resultState.cols.get(4).value should be (3)

    resultState.board.size should be (13)

    resultState.entireRowOrColMarked should be (true)
    resultState.winningScore should be (4512)
  }


}
