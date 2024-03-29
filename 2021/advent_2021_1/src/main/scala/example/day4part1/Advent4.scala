package example.day4part1

import cats.data._
import cats.implicits._

import scala.collection.immutable.List
import scala.io.Source

case class Board(board: Map[Int, List[(Int, Int)]], rows: Vector[Int], cols: Vector[Int], entireRowOrColMarked: Boolean) {

  def reduceList(key: Int, list: List[(Int, Int)]): Board = {
    val (updatedRow, updatedCol) = list.foldLeft((rows, cols)) {
      case (acc@(r, c), (x, y)) =>
        (r.get(x), c.get(y)).tupled.map { case (a, b) =>
          r.updated(x, a + 1) -> c.updated(y, b + 1)
        }.getOrElse(acc)
    }
    val allMarkedCount = rows.size
    val allOfARowOrColMarked = updatedRow.contains(allMarkedCount) || updatedCol.contains(allMarkedCount)
    copy(board - key, updatedRow, updatedCol, allOfARowOrColMarked)
  }

  def modify(key: Int): Board = board.get(key).fold(this)(list => reduceList(key, list))

  def winningScore(winningElem: Int): Int = board.keys.sum * winningElem
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

    new Board(board, rows, cols, false)
  }

  def markBoardElement(key: Int): BoardState[Unit] = State.modify(s => s.modify(key))

  def doTheMarking(board: Board, elem: Int) =
    markBoardElement(elem).runS(board).value

  def makeAnElementOfAllBoards(boards: List[Board], elem: Int): List[Board] =
    boards.traverse(board => List(doTheMarking(board, elem))).flatten

}

object Advent4 extends App {
  private val dataFile = "./src/main/resources/example/day4part1/sample.txt"
  val depthsList =
    Source.fromFile(dataFile)
      .getLines()
      .toList

  val (_, p) = depthsList.foldLeft((List.empty[String], List.empty[List[String]])) {
    case ((l, ls), s: String) => s match {
      case _ if s.isEmpty => (Nil, l.reverse :: ls)
      case _ => (s :: l, ls)
    }
  }

  p.foreach(println)
}
