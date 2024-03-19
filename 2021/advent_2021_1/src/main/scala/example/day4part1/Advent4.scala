package example.day4part1

import cats._
import cats.data._
import cats.implicits._

import scala.io.Source

case class BoardState(board: Map[Int, List[(Int, Int)]], rows: Vector[Int], cols: Vector[Int])

object BoardState {
  def apply(nRows: Int, nCols: Int, listOfElems: List[Int]): BoardState = {
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

    new BoardState(board, rows, cols)
  }

  def markElement(elem: Int, boardState: State[BoardState, BoardState]): State[BoardState, BoardState] = ???
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
