package example.day4part1

import cats.data._
import cats.implicits._

import scala.io.Source

case class Board(board: Map[Int, List[(Int, Int)]], rows: Vector[Int], cols: Vector[Int]) {

  def markACoordinate: State[Int, Board] = State { key =>
    board(key) match {
      case Nil =>
        (key, copy(board - key))

      case (x, y) :: xs =>
        val q = for {
          xV <- rows.get(x)
          yV <- cols.get(y)
        } yield (key, copy(board.updated(key, xs), rows.updated(x, xV+1), cols.updated(y, yV+1)))

        q.getOrElse(key, this)
    }
  }

  def reduceList(key: Int, list: List[(Int, Int)]): Board = {
    val (updatedRow, updatedCol) = list.foldLeft((rows, cols)) {
      case (acc@(r, c), (x, y)) =>
        (r.get(x), c.get(y)).mapN { (a, b) =>
          r.updated(x, a+1) -> c.updated(y, b+1)
        }.getOrElse(acc)
    }
    copy(board - key, updatedRow, updatedCol)
  }

  def modify(key: Int): Board = board.get(key) match {
    case None => this
    case Some(list) => reduceList(key, list)
  }

}

object Board {

  type BoardState[A] = State[Board, A]

  def apply(nRows: Int, nCols: Int, listOfElems: List[Int]): Board = {
    val listOfCoords = for {
      row <- (0 until nRows).toList
      col <- (0 until nCols).toList
    } yield List(row -> col)

    val listOfMaps = listOfElems.zip(listOfCoords).map { x =>
      Map(x._1 -> x._2)
    }

    val board = listOfMaps.combineAll
    val rows = Vector.fill(nRows)(0)
    val cols = Vector.fill(nCols)(0)

    new Board(board, rows, cols)
  }

  def markACoordinate(key: Int): BoardState[Unit] = State.modify { s =>
    val board = s.board
    board.get(key) match {
      case None => s
      case Some(x) => s.modify(key)
    }
  }
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
