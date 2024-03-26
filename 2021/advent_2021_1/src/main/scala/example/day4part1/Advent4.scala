package example.day4part1

import cats.data._
import cats.implicits._

import scala.io.Source

case class Board(board: Map[Int, List[(Int, Int)]], rows: Vector[Int], cols: Vector[Int]) {

  def reduceList(key: Int, list: List[(Int, Int)]): Board = {
    val (updatedRow, updatedCol) = list.foldLeft((rows, cols)) {
      case (acc@(r, c), (x, y)) =>
        (r.get(x), c.get(y)).mapN { (a, b) =>
          r.updated(x, a + 1) -> c.updated(y, b + 1)
        }.getOrElse(acc)
    }
    copy(board - key, updatedRow, updatedCol)
  }

  def modify(key: Int): Board = board.get(key).fold(this)(list => reduceList(key, list))

}

object Board {

  type BoardState[A] = State[Board, A]

  def createCoordMapping(nRows: Int, nCols: Int, listOfElems: List[Int]) = {
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

  def markACoordinate(key: Int): BoardState[Unit] = State.modify(s => s.modify(key))
}

object Advent4 extends App {
  def transpose(list: List[Int]) = {
    val tmpList = list.zipWithIndex

    val resultList = for {
      i <- (0 to 4).toList
      (elem, idx) <- tmpList
      if i == idx % 5
    } yield elem

    resultList
  }

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
