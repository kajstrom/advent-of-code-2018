package day01

import utils.time.time


object day01 extends App{
  val input = scala.io.Source.fromResource("day01.txt")
    .getLines()
    .toList
    .map(_.toInt)

  println("Part 1:", time {input.sum})

  var freqsSeen: List[Int] = List(0)

  var currentFreq = 0
  var idx = 0
  time {
    while (!freqsSeen.tail.contains(currentFreq)) {
      currentFreq = currentFreq + input(idx)
      freqsSeen = freqsSeen.::(currentFreq)

      idx = (idx + 1) % input.length
    }
  }

  println("Part 2:", currentFreq)
}
