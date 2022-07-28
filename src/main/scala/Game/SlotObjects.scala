package Game

object SlotObjects {

  trait SlotEnvironment
  case class Column(elements: List[Objects]) extends SlotEnvironment
  case class Screen(screen: List[Column]) extends SlotEnvironment
  case class Configure(configure: Map[Objects, Int]) extends SlotEnvironment {
    def apply(value: Map[Objects, Int]): Configure = Configure(value)
  }

  trait Objects
  case object Point5 extends Objects
  case object Point10 extends Objects
  case object Sword extends Objects
  case object Bag extends Objects
  case object MiniGame extends Objects
  case object Chest extends Objects
  case object Jackpot extends Objects
  case object FreeSpins extends Objects
  case object Multiply extends Objects
  case object Action extends Objects
}
