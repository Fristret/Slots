package Game

object SlotObjects {

  sealed trait SlotEnvironment
  final case class Column(elements: Map[Int, Element] = Map.empty[Int, Element]) extends SlotEnvironment
  final case class Screen(value: List[Column]) extends SlotEnvironment
  final case class Configure(value: List[List[Element]]) extends SlotEnvironment
  final case class Payment(win: Int, configure: Configure) extends SlotEnvironment

  sealed trait Element
  final case object Point5 extends Element
  final case object Point10 extends Element
  final case object Sword extends Element
  final case object Bag extends Element
  final case object MiniGame extends Element
  final case object Chest extends Element
  final case object Jackpot extends Element
  final case object FreeSpins extends Element
  final case object Action extends Element
  final case object Wild extends Element
  final case object NoElement extends Element
}
