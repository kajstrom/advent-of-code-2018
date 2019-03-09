package day02

import utils.time

object day02 extends App {
  val input = scala.io.Source.fromResource("day2.txt")
    .getLines()
    .toList
    .map(_.toCharArray)

  time.time {
    val checksum = input
      .map((chars: Array[Char]) => chars.groupBy(c => c))
      .map(_.mapValues(_.length))
      .flatMap(_.values.toList.distinct)
      .filter(count => count == 2 || count == 3)
      .groupBy(count => count)
      .mapValues(_.length)
      .values
      .toList
      .product

    println("Part 1", checksum)
  }


}
