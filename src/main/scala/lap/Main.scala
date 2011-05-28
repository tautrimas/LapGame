package lap

import util.Random
import scala.Some
import collection.mutable.{HashSet, HashMap, ArrayBuffer, Queue}

class Map(size: Int) {

  assert(size % 2 == 0)

  val tileCount = size * size

  val map = Array.fill(size, size)(0)

//  def totalyRandom() {
//    val randomSet = Random.shuffle((0 until tileCount).toSeq)
//    randomSet.zipWithIndex foreach { case (i, index) =>
//      map(i % size)(i / size) = index / (tileCount / 4)
//    }
//  }

  def totallyOrdered() {
    (0 until tileCount) foreach { i =>
      val x = i % size
      val y = i / size
      map(x)(y) = i / (tileCount / 4) + 1
    }
  }

//  def relaxAll() {
//    var loner = locateLoner()
//    while (loner.isDefined) {
//      val pairLoner = locateLoner(loner.get)
//      if (pairLoner.isDefined) {
//        val temp: Int = map(loner.get._1)(loner.get._2)
//        map(loner.get._1)(loner.get._2) =
//            map(pairLoner.get._1)(pairLoner.get._2)
//        map(pairLoner.get._1)(pairLoner.get._2) = temp
//        println(loner, pairLoner)
//      }
//      else {
//        val pairLoner = Some(Random.nextInt(size), Random.nextInt(size))
//        val temp: Int = map(loner.get._1)(loner.get._2)
//        map(loner.get._1)(loner.get._2) =
//            map(pairLoner.get._1)(pairLoner.get._2)
//        map(pairLoner.get._1)(pairLoner.get._2) = temp
//      }
//      loner = locateLoner()
//      printMap()
//      println()
//      Console.readLine()
//    }
//  }

  def slideSequence() {
    var soldierTemp = (Random.nextInt(size), Random.nextInt(size))
    while (doesBreakChain(soldierTemp, 0)) {
      soldierTemp = (Random.nextInt(size), Random.nextInt(size))
    }
    val soldier = soldierTemp
    var hand = access(soldier)
    access(soldier, 0)

    var previous = soldier
    var pos = randomDirection(previous)
    while (pos != soldier) {
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
    access(pos, hand)
  }

  def doesBreakChain(pos: (Int, Int), hand: Int): Boolean = {
    val tempSave = access(pos)
    access(pos, hand)
    val allGood = bfs(pos)
    access(pos, tempSave)
    !allGood
  }

  def bfs(source: (Int, Int)): Boolean = {
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
    println(("bfs", source, access(source), groups))
    val res = groups.forall { case (color, set) =>
      if (color == 0) true
      else set.size + 1 >= tileCount / 4
    }
    println(res)
    printMap()
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

//  def locateLoner(exceptPair: (Int, Int) = null): Option[(Int, Int)] = {
//    Random.shuffle(0 until tileCount) foreach { i =>
//      if (!(exceptPair != null &&
//          i == exceptPair._1 + exceptPair._2 * size)) {
//        val res = isLoner(i)
//        if (res.isDefined)
//          return res
//      }
//    }
//    None
//  }

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
        print(map(x)(y) + " ")
      }
      println()
    }
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
}

object Main extends App {
  val map = new Map(8)
  map.totallyOrdered()
  map.printMap()
  (1 to 5) foreach { _ => map.slideSequence() }
//  map.slideSequence()
  map.printMap()
}
