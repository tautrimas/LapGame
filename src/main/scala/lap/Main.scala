package lap

object Main extends App {
  val map = new Map(8)
  var score = 0
  while (score < 91 || map.hand != 0) {
    map.totallyOrdered()
    map.slideSequence()
//    if (map.getScore() >= 90 && map.hand == 0) {
//      map.printMap()
//      map.printStats()
//    }
    score = map.getScore()
  }

  println("I'm ready. You are sooo f**ing dead!")

  var stop = false
  var guesses = 0
  while (!stop) {
    println("Coordinates:")
    try {
      val x1 = Console.readInt() -1
      val x2 = x1 + 1
      val y1 = Console.readInt() -1
      val y2 = y1 + 1

      println("You have chosen:", x1, y1)

      val tiles = List(
        map.map(x1)(y1),
        map.map(x2)(y1),
        map.map(x1)(y2),
        map.map(x2)(y2)
      )
      tiles.groupBy(x => x).map(p => p).toArray.sortBy {case (color, list) => color } foreach { case (c, set) =>
        println("count, color", set.size, c + 1)
      }
      println("Enter \"exit\" text to exit. Enter to continue:")
      if (Console.readLine() == "exit")
        stop = true
      guesses += 1
    }
    catch {
      case e => {
        println("Exception: " + e)
        println("Try again")
      }
    }
  }
  map.printMap()
  println(map.getScore())
  println("guess count", guesses)
}
