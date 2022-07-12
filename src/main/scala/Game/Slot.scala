package Game

object Slot {
  trait SlotEnvironment
  case class Column(first: Objects, second: Objects, third: Objects) extends SlotEnvironment
  case class Result(line: List[Objects]) extends SlotEnvironment


  trait Objects
  case class Point(points: Int) extends Objects
  case class Sword(sword: String) extends Objects
  case class Bag(bag: String) extends Objects
  case class MiniGame(miniGame: String) extends Objects
  case class Chest(chest: String) extends Objects
  case class Jackpot(jackpot: String) extends Objects
  case class FreeSpins(freeSpins: String) extends Objects
}
