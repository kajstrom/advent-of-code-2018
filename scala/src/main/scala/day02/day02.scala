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

  def isCorrectId(a: Array[Char], b: Array[Char]): Boolean = {
    val combined = (a zip b)
      .map(pair => pair._1 == pair._2)
      .filter(!_)

    combined.length == 1
  }

  time.time {
    val correctIds = input.foldLeft(List[Array[Char]]()) {
      (acc: List[Array[Char]], id: Array[Char]) => {
        if (acc.length == 2) {
          acc
        } else {
          val aCorrectId = input.filter(anotherId => isCorrectId(id, anotherId))

          if (aCorrectId.nonEmpty) {
            acc.::(id).::(aCorrectId.head)
          } else {
            acc
          }
        }
      }
    }

    println("Part 2", correctIds.head.zip(correctIds(1)).filter(c => c._1 == c._2).map(_._1).mkString("") )
  }
}
