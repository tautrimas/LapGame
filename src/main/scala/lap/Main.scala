package lap

import util.Random
import scala.Some


class Map(size: Int) {

  assert(size % 2 == 0)

  val tileCount = size * size

  val map = Array.fill(size, size)(0)

  def totalyRandom() {
    val randomSet = Random.shuffle((0 until tileCount).toSeq)
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

  def relaxAll() {
    var lonerExists = true
    while (lonerExists) = 
  }

  def locateLoner() {
    (0 until tileCount) foreach { i =>
      val res = isLoner(i)
      if (res.isDefined)
        return res
    }
  }

  def isLoner(i: Int): Option[(Int, Int)] = {
    val x = i % size
    val y = i / size
    val sector = map(x)(y)
    var loner = true
    if (x >= 1) loner = loner && map(x - 1)(y) != sector
    if (x < size - 1) loner = loner && map(x + 1)(y) != sector
    if (y >= 1) loner = loner && map(x)(y - 1) != sector
    if (y < size - 1) loner = loner && map(x)(y + 1) != sector
    if (loner)
      Some((x, y))
    else None
  }
}

object Main extends App {
  val map = new Map(8)
  map.totalyRandom()
  map.printMap()
}
