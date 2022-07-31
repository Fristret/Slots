package Game

object SlotObjects {

  trait SlotEnvironment
  case class Column(elements: Map[Int, Element] = Map.empty[Int, Element]) extends SlotEnvironment
  case class Screen(value: List[Column]) extends SlotEnvironment
  case class Configure(value: List[List[Element]]) extends SlotEnvironment
  case class Payment(win: Int, configure: Configure) extends SlotEnvironment

  trait Element
  case object Point5 extends Element
  case object Point10 extends Element
  case object Sword extends Element
  case object Bag extends Element
  case object MiniGame extends Element
  case object Chest extends Element
  case object Jackpot extends Element
  case object FreeSpins extends Element
  case object Action extends Element
  case object Wild extends Element
  case object NoElement extends Element
}
