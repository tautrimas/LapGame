package lap

import util.Random
import scala.Some
import collection.mutable.ArrayBuffer


class Map(size: Int) {

  assert(size % 2 == 0)

  val tileCount = size * size

  val map = Array.fill(size, size)(0)

  def clear() {
    for (y <- 0 until size) {
      for (x <- 0 until size) {
        map(x)(y) = 0
      }
    }
  }

  def grow() {
    var pairs = ArrayBuffer[(Int, Int)]()
    (1 to 4) foreach { _ =>
      var pair1 = (Random.nextInt(size), Random.nextInt(size))
      while (pairs contains pair1) {
        pair1 = (Random.nextInt(size), Random.nextInt(size))
      }
      pairs += pair1
    }

    val sectors = ArrayBuffer.fill(4)(ArrayBuffer[(Int, Int)]())

    pairs.zipWithIndex foreach { case (pair, i) =>
      sectors(i) += pair
      map(pair._1)(pair._2) = i + 1
    }


    while (sectors(3).size != tileCount / 4) {
      sectors.zipWithIndex foreach { case (sector, i) =>
        var shouldContinue = true
        var failCount = 0
        while (shouldContinue && failCount < 5000) {
          val pair = sector(Random.nextInt(sector.size))
          val direction = isFree(pair._1, pair._2)
          if (direction.isDefined) {
            val (x, y) = direction.get
            map(x)(y) = i + 1
            sector += direction.get
            shouldContinue = false
            failCount = 0
          }
          failCount += 1
        }
        if (failCount == 5000) {
          clear()
          grow()
          return
        }
      }
    }
  }

  def isFree(x: Int, y: Int): Option[(Int, Int)] = {
    val directions = Random.shuffle(
      ArrayBuffer((x-1, y), (x+1, y), (x, y-1), (x, y+1)))
    directions foreach { case (dx, dy) =>
      try
        if (map(dx)(dy) == 0)
          return Some((dx, dy))
      catch {
        case e: IndexOutOfBoundsException => {}
      }
    }
    None
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
  map.grow()
  map.printMap()
}
