package example.day4part1

import cats.Alternative
import cats.implicits._

object MyTry1 {
  def addIfNotTooBig(acc: Int, x: Int): Option[Int] = if (x > 8) none[Int] else (acc + x).some

  def parseInt(x: String): Either[Throwable, Int] = Either.catchNonFatal(x.toInt)

  type Mo[A] = Either[Throwable, A]

  def processStrings(list: List[Double], string: String): Either[Throwable, List[Double]] =
    (list, string) match {
      // as we see operators we take  2 most recent numbers from the list and process
      case (x :: y :: tail, "+") => println("+"); Either.right((y + x) :: tail)
      case (x :: y :: tail, "-") => println("-"); Either.right((y - x) :: tail)
      case (x :: y :: tail, "*") => println("*"); Either.right((y * x) :: tail)
      // as we see digits we add them to front of list
      case (xs, number) => println(s"prepend $number"); parseInt(number).map(_ :: xs)
    }

  def solve(string: String): Either[Throwable, Double] = ???
//  = {
//    val list: List[String] = string.split(' ').toList
//
//    for {
//      List(x) <- list.foldM(Nil: List[Double]) { x => processStrings(Nil, "")
//
//      }
//    } yield x
//  }

  def even(i: Int): Option[String] = Alternative[Option].guard(i % 2 == 0).as("even")
  def evenElems(list: List[Int]): List[List[Int]] = Alternative[List].guard(list.exists(_ % 2 == 0)).as(list)

  def main(args: Array[String]): Unit = {
    val p = List.range(1, 10)
    println(evenElems(p))
//    val out = (List(1, 9, 2, 3).foldM(0)(addIfNotTooBig))
//    println(out)
//
//    val out2 = (List(1, 7, 2, 3).foldM(0)(addIfNotTooBig))
//    println(out2)
//
//    println("calculator ok")
//    println(solve("11 3 4 + 3 * -"))
//
//    println("calculator bad")
//    println(solve("11s 3 4 + 3 * -"))
//
//    val exists: Option[Boolean] = List(1, 2, 3).existsM(i => (i > 2).some)
//    println(exists)
//
//    val exists2: List[Boolean] = List(1, 2, 3).existsM(i => List(i > 2))
//    println(exists2)
//
//    val exists3: List[Boolean] = 3.some.existsM(i => List(i > 2))
//    println(exists3)
  }
}
