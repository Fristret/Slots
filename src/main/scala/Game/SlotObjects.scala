package Game

object SlotObjects {

  trait SlotEnvironment
  case class Column(elements: List[Element]) extends SlotEnvironment
  case class Screen(screen: List[Column]) extends SlotEnvironment
  case class Configure(configure: Map[Element, Int]) extends SlotEnvironment {
    def apply(value: Map[Element, Int]): Configure = Configure(value)
  }

  trait Element
  case object Point5 extends Element
  case object Point10 extends Element
  case object Sword extends Element
  case object Bag extends Element
  case object MiniGame extends Element
  case object Chest extends Element
  case object Jackpot extends Element
  case object FreeSpins extends Element
  case object Multiply extends Element
  case object Action extends Element
}
