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
    print("0 ")
    (0 until size) foreach { i => print(i + " ")}
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
    var loner = locateLoner()
    while (loner.isDefined) {
      val pairLoner = locateLoner(loner.get)
      if (pairLoner.isDefined) {
        val temp: Int = map(loner.get._1)(loner.get._2)
        map(loner.get._1)(loner.get._2) =
            map(pairLoner.get._1)(pairLoner.get._2)
        map(pairLoner.get._1)(pairLoner.get._2) = temp
        println(loner, pairLoner)
      }
      else {
        val pairLoner = Some(Random.nextInt(size), Random.nextInt(size))
        val temp: Int = map(loner.get._1)(loner.get._2)
        map(loner.get._1)(loner.get._2) =
            map(pairLoner.get._1)(pairLoner.get._2)
        map(pairLoner.get._1)(pairLoner.get._2) = temp
      }
      loner = locateLoner()
      printMap()
      println()
      Console.readLine()
    }
  }

  def locateLoner(exceptPair: (Int, Int) = null): Option[(Int, Int)] = {
    Random.shuffle(0 until tileCount) foreach { i =>
      if (!(exceptPair != null &&
          i == exceptPair._1 + exceptPair._2 * size)) {
        val res = isLoner(i)
        if (res.isDefined)
          return res
      }
    }
    None
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
  map.relaxAll()
  map.printMap()
}
