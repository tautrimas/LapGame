package lap

object Main extends App {
  val map = new Map(8)
  (1 to 500) foreach { _ =>
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
