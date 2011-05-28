package lap

import util.Random


class Map(size: Int) {

  assert(size % 2 == 0)

  val tileCount = size * size

  val map = Array.fill(size, size)(0)

  def totalyRandom() {
    val randomSet = Random.shuffle((0 until size).toSeq)
    randomSet.zipWithIndex foreach { case (i, index) =>
      map(i % size)(i / size) = index / (tileCount / 4)
    }
  }

  def printMap() {
    (0 to size) foreach { i => print(i + " ")}
    println()
    for (y <- 0 until size) {
      print("abcdefghijklmnopqrstuv".charAt(y) + " ")
      for (x <- 0 until size) {
        print(map(x)(y) + " ")
      }
      println()
    }
  }
}

object Main extends App {
  val map = new Map(8)
  map.totalyRandom()
  map.printMap()
}
