package example.day4part1

import cats.data._
import cats.implicits._

import scala.io.Source

case class Board(board: Map[Int, List[(Int, Int)]],
                 rows: Vector[Int],
                 cols: Vector[Int],
                 winningBoardElemOpt: Option[Int] = None) {

  private def reduceList(key: Int, list: List[(Int, Int)]): Board = {
    val (updatedRow, updatedCol) = list.foldLeft((rows, cols)) {
      case (acc@(r, c), (x, y)) =>
        (r.get(x), c.get(y)).tupled.map { case (a, b) =>
          r.updated(x, a + 1) -> c.updated(y, b + 1)
        }.getOrElse(acc)
    }
    val allMarkedCount = rows.size
    val allOfARowOrColMarked = updatedRow.contains(allMarkedCount) || updatedCol.contains(allMarkedCount)
    if (allOfARowOrColMarked)
      copy(board - key, updatedRow, updatedCol, key.some)
    else
      copy(board - key, updatedRow, updatedCol)
  }

  def modify(elem: Int): Board = board.get(elem).fold(this)(list => reduceList(elem, list))

  def winningScore: Int = winningBoardElemOpt.fold(0)(winningElem => board.keys.sum * winningElem)

  def isItAWinnerBoard: Boolean = winningBoardElemOpt.isDefined
}

object Board {

  type BoardState[A] = State[Board, A]

  private def createCoordMapping(nRows: Int, nCols: Int, listOfElems: List[Int]) = {
    val listOfCoords = for {
      row <- List.range(0, nRows)
      col <- List.range(0, nCols)
    } yield List(row -> col)

    listOfElems.zip(listOfCoords).foldMap(x => Map(x._1 -> x._2))
  }

  def apply(nRows: Int, nCols: Int, listOfElems: List[Int]): Board = {
    val board = createCoordMapping(nRows, nCols, listOfElems)
    val rows = Vector.fill(nRows)(0)
    val cols = Vector.fill(nCols)(0)

    new Board(board, rows, cols)
  }

  def markBoardElement(elem: Int): BoardState[Unit] = State.modify(s => s.modify(elem))

  private def doTheMarking(board: Board, elem: Int) =
    markBoardElement(elem).runS(board).value

  def makeAnElementOfAllBoards(boards: List[Board], elem: Int): List[Board] =
    boards.traverse(board => List(doTheMarking(board, elem))).flatten

  private def markElemAndCheckAllBoards(boards: List[Board], elem: Int): List[Board] = {
    val p = makeAnElementOfAllBoards(boards, elem)
    p.find(_.isItAWinnerBoard).fold(p)(board => List(board))
  }

  private def markElemInBoard(acc: List[Board], elem: Int): Option[List[Board]] = { // x.find(_.isItAWinnerBoard)
    val winningBoardOpt = acc.headOption.find(_.isItAWinnerBoard)
    winningBoardOpt.map(board => List(board)) orElse markElemAndCheckAllBoards(acc, elem).some
  }

  def processDrawnSeriesOfNumbers(boards: List[Board], elementsDrawn: List[Int]): Option[Board] =
    elementsDrawn.foldM(boards)(markElemInBoard).fold(none[Board])(_.headOption)
}

object Advent4 extends App {
  private val dataFile = "./src/main/resources/example/day4part1/example_data.txt"
  private val source = Source.fromFile(dataFile)
  val boardElementsList =
    source
      .getLines()
      .toList

  val (_, inputBoards) = boardElementsList.foldLeft((List.empty[String], List.empty[List[String]])) {
    case ((l, ls), s: String) => s match {
      case _ if s.isEmpty => (Nil, l.reverse :: ls)
      case _ => (s :: l, ls)
    }
  }

  val listOfBoards = inputBoards.map { r =>
    val p = for {
      l <- r
      s <- l.split("\\s+")
      if s.nonEmpty
    } yield s
    p.map(_.toInt)
  }.map { boardData =>
    Board(5, 5, boardData)
  }

  //  val drawnNumbers = List(7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1)

  val drawnNumbers = List(46, 79, 77, 45, 57, 34, 44, 13, 32, 88, 86, 82, 91, 97, 89, 1, 48, 31, 18, 10,
    55, 74, 24, 11, 80, 78, 28, 37, 47, 17, 21, 61, 26, 85, 99, 96, 23, 70, 3, 54, 5, 41, 50, 63, 14, 64, 42, 36,
    95, 52, 76, 68, 29, 9, 98, 35, 84, 83, 71, 49, 73, 58, 56, 66, 92, 30, 51, 20, 81, 69, 65, 15, 6, 16, 39, 43,
    67, 7, 59, 40, 60, 4, 90, 72, 22, 0, 93, 94, 38, 53, 87, 27, 12, 2, 25, 19, 8, 62, 33, 75)

  val winningBoard = Board.processDrawnSeriesOfNumbers(listOfBoards, drawnNumbers)

  winningBoard.fold(println("No Winning board found"))(winningBoard => println(winningBoard.winningScore))
}
