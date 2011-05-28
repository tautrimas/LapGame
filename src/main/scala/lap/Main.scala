package lap

import util.Random
import scala.Some
import collection.mutable.{HashSet, HashMap, ArrayBuffer, Queue}

class Map(size: Int) {

  assert(size % 2 == 0)

  val tileCount = size * size

  val map = Array.fill(size, size)(0)

  var hand = 0

  def totallyOrdered() {
    (0 until tileCount) foreach { i =>
      val x = i % size
      val y = i / size
      map(x)(y) = i / (tileCount / 4) + 1
    }
  }

  def slideSequence() {
    var soldierTemp = (Random.nextInt(size), Random.nextInt(size))
    while (doesBreakChain(soldierTemp, 0)) {
      soldierTemp = (Random.nextInt(size), Random.nextInt(size))
    }
    val soldier = soldierTemp
    hand = access(soldier)
    access(soldier, 0)

    var previous = soldier
    var pos = randomDirection(previous)
    var stepsPassed = 0
    while (stepsPassed < 50000) {
      stepsPassed += 1
//      if (stepsPassed % 25000 == 0) {
//        printMap()
//        println()
//      }
      //      println(pos, hand)
      //      printMap()
//      Console.readLine()
      if (!doesBreakChain(pos, hand)) {
        val temp = access(pos)
        access(pos, hand)
        hand = temp
        previous = pos
        pos = randomDirection(previous)
      }
      else {
        pos = randomDirection(previous)
      }
    }
  }

  def doesBreakChain(pos: (Int, Int), hand: Int): Boolean = {
    val tempSave = access(pos)
    access(pos, hand)
    val allGood = bfs(pos, tempSave)
    access(pos, tempSave)
    !allGood
  }

  def bfs(source: (Int, Int), hand: Int): Boolean = {
    val queue = Queue[(Int, Int)](source)
    val visited = HashSet[(Int, Int)](source)
    tileNeighs(source) foreach { neigh =>
      if ( !(visited.map(p => access(p)) contains access(neigh)) ) {
        queue.enqueue(neigh)
        visited += neigh
      }
    }
    while (!queue.isEmpty) {
      val u = queue.dequeue()
      tileNeighs(u).foreach { case v =>
        if ( access(v) == access(u) && !(visited contains v)) {
          visited += v
          queue.enqueue(v)
        }
      }
    }
    val groups = visited.groupBy { pos => access(pos) }
//    println(("bfs", source, access(source), groups))
    val res = groups.forall { case (color, set) =>
      if (color == 0) true
      else
        if (color == hand)
          set.size + 1 == tileCount / 4
        else
          set.size == tileCount / 4
    }
//    println(res)
//    printMap()
    res
  }

  def tileNeighs(pos: (Int, Int)): ArrayBuffer[(Int, Int)] = {
    var positions = ArrayBuffer[(Int, Int)]()
    val (x, y) = pos
    if (x > 0)
      positions += ((x - 1, y))
    if (x < size - 1)
      positions += ((x + 1, y))
    if (y > 0)
      positions += ((x, y - 1))
    if (y < size - 1)
      positions += ((x, y + 1))

    positions
  }

  def randomDirection(pos: (Int, Int)): (Int, Int) = {
    val positions = tileNeighs(pos)
    positions( Random.nextInt(positions.size) )
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

  def printMap() {
    print("0 ")
    (0 until size) foreach { i => print(i + " ")}
    println()
    for (y <- 0 until size) {
      print("0123456789".charAt(y) + " ")
      for (x <- 0 until size) {
        print("[iHOs".charAt(map(x)(y)) + " ")
      }
      println()
    }
    if (hand != 0) println("hand", "[iHOs".charAt(hand))
    else println()
  }

  def access(pair: (Int, Int)) = {
    val (x, y) = pair
    map(x)(y)
  }

  def access(pair: (Int, Int), v: Int) {
    val (x, y) = pair
    map(x)(y) = v
  }

  def access(i: Int) = {
    val x = i % size
    val y = i / size
    map(x)(y)
  }

  def access(i: Int, v: Int) = {
    val x = i % size
    val y = i / size
    map(x)(y) = v
  }

  def printStats() {
    val stats = Array.fill(4)(0)
    for (y <- 0 until size) {
      for (x <- 0 until size) {
        val sector = (x + y * size) / (tileCount / 4) + 1
        if (map(x)(y) == sector) {
          stats(sector - 1) += 1
        }
      }
    }
    println(stats.mkString(" "))
    val minimum = 4 * (tileCount / 4) / 4
    println(
      "Score min max real",
      -minimum,
      tileCount - minimum,
      stats.sum - minimum)
  }

  def getScore(): Int = {
    val stats = Array.fill(4)(0)
    for (y <- 0 until size) {
      for (x <- 0 until size) {
        val sector = (x + y * size) / (tileCount / 4) + 1
        if (map(x)(y) == sector) {
          stats(sector - 1) += 1
        }
      }
    }
    stats.sum - 4 * (tileCount / 4) / 4
  }
}

object Main extends App {
  val map = new Map(8)
  (1 to 10) foreach { _ =>
    map.totallyOrdered()
    map.slideSequence()
    if (map.getScore() < 8) {
      map.printMap()
      map.printStats()
    }
  }
//  map.slideSequence()
//  map.printMap()
//  map.printStats()
}
