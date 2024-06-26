package example.day4part1

import cats.data.State
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import cats.implicits._
import example.day4part1.Board.BoardState
import org.scalatest.OptionValues.convertOptionToValuable

class BoardSpec extends AnyFlatSpec with should.Matchers {
  def fixture = new {
    val boardData1: List[Int] = List(
      14, 21, 17, 24, 4,
      10, 16, 15, 9, 19,
      18, 8, 23, 26, 20,
      22, 11, 13, 6, 5,
      2, 0, 12, 3, 7
    )

    val boardData2: List[Int] = List(
      14, 21, 13, 24, 4,
      13, 16, 15, 9, 19,
      18, 8, 23, 26, 20,
      22, 11, 13, 6, 5,
      2, 0, 12, 3, 4
    )

    val zerothRowAll14: List[Int] = List(
      14, 14, 14, 14, 14,
      13, 16, 15, 9, 19,
      18, 8, 23, 26, 20,
      22, 11, 13, 6, 5,
      2, 0, 12, 3, 4
    )

    val zerothColAll14: List[Int] = List(
      14, 21, 13, 24, 4,
      14, 16, 15, 9, 19,
      14, 8, 23, 26, 20,
      14, 11, 13, 6, 5,
      14, 0, 12, 3, 4
    )

    val listOfBoards: List[Board] = List(
      List(
        22, 13, 17, 11, 0,
        8, 2, 23, 4, 24,
        21, 9, 14, 16, 7,
        6, 10, 3, 18, 5,
        1, 12, 20, 15, 19
      ),
      List(
        3, 15, 0, 2, 22,
        9, 18, 13, 17, 5,
        19, 8, 7, 25, 23,
        20, 11, 10, 24, 4,
        14, 21, 16, 12, 6
      ),
      List(
        14, 21, 17, 24, 4,
        10, 16, 15, 9, 19,
        18, 8, 23, 26, 20,
        22, 11, 13, 6, 5,
        2, 0, 12, 3, 7
      )
    ).map(boardData => Board(5, 5, boardData))
  }

  "A board representation" should "return proper coordinates" in {
    val f = fixture
    val boardData = f.boardData1

    val board = Board(5, 5, boardData)

    board.board(14) should be(List(0 -> 0))
    board.board(11) should be(List(3 -> 1))
    board.board(3) should be(List(4 -> 3))
  }

  "A board representation with duplicate numbers" should "return proper coordinates" in {
    val f = fixture
    val boardData = f.boardData2

    val board = Board(5, 5, boardData)

    board.board(4) should be(List(0 -> 4, 4 -> 4))
    board.board(13) should be(List(0 -> 2, 1 -> 0, 3 -> 2))
  }

  "Marking an element" should "update the coordinates vectors" in {
    val f = fixture
    val boardData = f.boardData2

    val board = Board(5, 5, boardData)
    val initialSize = board.board.size

    val startingState: BoardState[Unit] = Board.markBoardElement(14)

    val resultState: Board = startingState.runS(board).value

    resultState.rows.get(0).value should be(1)
    resultState.cols.get(0).value should be(1)
    resultState.board.size should be(initialSize - 1)

    resultState.winningBoardElemOpt should be(None)
  }

  "Marking an element so a single row" should "is completely marked" in {
    val f = fixture
    val boardData = f.zerothRowAll14

    val board = Board(5, 5, boardData)
    val initialSize = board.board.size

    val startingState: BoardState[Unit] = Board.markBoardElement(14)

    val resultState: Board = startingState.runS(board).value

    resultState.rows should contain theSameElementsAs Vector(5, 0, 0, 0, 0)
    resultState.cols should contain only(1)

    resultState.board.size should be(initialSize - 1)

    resultState.winningBoardElemOpt.isDefined should be(true)
  }

  "Marking an element so a single column" should "is completely marked" in {
    val f = fixture
    val boardData = f.zerothColAll14

    val board = Board(5, 5, boardData)
    val initialSize = board.board.size

    val startingState: BoardState[Unit] = Board.markBoardElement(14)

    val resultState: Board = startingState.runS(board).value

    resultState.cols should contain theSameElementsAs Vector(5, 0, 0, 0, 0)
    resultState.rows should contain only(1)

    resultState.board.size should be(initialSize - 1)

    resultState.winningBoardElemOpt.isDefined should be(true)
    resultState.winningBoardElemOpt.value should be (14)
    resultState.winningScore should be(3290)
  }

  "Marking a series of elements so a single row" should "is completely marked" in {
    val f = fixture
    val boardData = f.boardData1

    val board = Board(5, 5, boardData)

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

    val resultState = finalState.runS(board).value

    resultState.rows should contain theSameElementsAs Vector(5, 1, 1, 2, 3)
    resultState.cols should contain theSameElementsAs Vector(2, 3, 2, 2, 3)

    resultState.board.size should be(13)

    resultState.winningBoardElemOpt.isDefined should be(true)
    resultState.winningScore should be(4512)
  }

  "Drawing elements for a set of boards" should "calculate the right score for the winning board" in {
    val f = fixture
    val inputBoards = f.listOfBoards

    val result = Board.makeAnElementOfAllBoards(inputBoards, 7)

    result.size should be(3)

    val (updatedBoard0, updatedBoard1, updatedBoard2) = (result.get(0), result.get(1), result.get(2))

    updatedBoard0.value.rows(2) should be(1)
    updatedBoard0.value.cols(4) should be(1)

    updatedBoard1.value.rows(2) should be(1)
    updatedBoard1.value.cols(2) should be(1)

    updatedBoard2.value.rows(4) should be(1)
    updatedBoard2.value.cols(4) should be(1)
  }

  "Drawing a series of elements for a set of boards" should "returns one winning board" in {
    val f = fixture
    val inputBoards = f.listOfBoards

    val drawnNumbers = List(7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1)

    val winningBoard: Option[Board] = Board.processDrawnSeriesOfNumbers(inputBoards, drawnNumbers)

    winningBoard.value.winningScore should be (4512)
  }
}
