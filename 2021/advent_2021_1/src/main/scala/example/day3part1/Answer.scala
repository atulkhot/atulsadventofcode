package example.day3part1

import scala.io.Source

object Answer {
  type BitLine = IndexedSeq[Int]

  def parseBitLine(line: String): BitLine =
    line.map(c => c - '0') // 1 or 0

  private val dataFile = "./src/main/resources/example/day3part1/example_data.txt"
  //  private val dataFile = "./src/main/resources/example/day3part1/sample.txt"

  private val input =
    Source.fromFile(dataFile)
      .getLines()
      .toList

  val bitLines: List[BitLine] = input.map(parseBitLine)

  val sumOfOneBits: IndexedSeq[Int] = bitLines.reduceLeft((prevSum, line) =>
    for ((prevBitSum, lineBit) <- prevSum.zip(line))
      yield prevBitSum + lineBit
  )

  val total = bitLines.size

  val gammaRateBits: BitLine =
    for (sumOfOneBit <- sumOfOneBits)
      yield (if (sumOfOneBit * 2 > total) 1 else 0)
  val gammaRate = bitLineToInt(gammaRateBits)

  val epsilonRateBits: BitLine =
    for (sumOfOneBit <- sumOfOneBits)
      yield (if (sumOfOneBit * 2 < total) 1 else 0)
  val epsilonRate = bitLineToInt(epsilonRateBits)


  def bitLineToInt(bitLine: BitLine): Int =
    Integer.parseInt(bitLine.mkString, 2)

  def oxygen_gen_partition(bitLineSeq: List[BitLine], bitIndex: Int): BitLine = {
    if (bitLineSeq.length == 1)
      bitLineSeq.head
    else {
      val (a, b) = bitLineSeq.partition(p => p.lift(bitIndex).contains(1))
      if (a.length >= b.length)
        oxygen_gen_partition(a, bitIndex + 1)
      else
        oxygen_gen_partition(b, bitIndex + 1)
    }
  }

  def co2_scrubber_partition(bitLineSeq: List[BitLine], bitIndex: Int): BitLine = {
    if (bitLineSeq.length == 1)
      bitLineSeq.head
    else {
      val (a, b) = bitLineSeq.partition(p => p.lift(bitIndex).contains(0))
      if (a.length <= b.length)
        co2_scrubber_partition(a, bitIndex + 1)
      else
        co2_scrubber_partition(b, bitIndex + 1)
    }
  }

  def main(args: Array[String]): Unit = {

    val oxygen_rating = bitLineToInt(oxygen_gen_partition(bitLines, 0))
    val co2_scubber_rating = bitLineToInt(co2_scrubber_partition(bitLines, 0))
    println(oxygen_rating * co2_scubber_rating)
  }
}
