package game.models

object SlotObjects {

  sealed trait SlotEnvironment
  final case class Column(elements: Map[Int, Symbol]) extends SlotEnvironment
  final case class Reel(column: List[Column]) extends SlotEnvironment

  sealed trait Symbol
  final case object Point5 extends Symbol
  final case object Point10 extends Symbol
  final case object Sword extends Symbol
  final case object Bag extends Symbol
  final case object MiniGame extends Symbol
  final case object Chest extends Symbol
  final case object Jackpot extends Symbol
  final case object FreeSpins extends Symbol
  final case object Action extends Symbol
  final case object Wild extends Symbol
  final case object NoSymbol extends Symbol

  final case class PayLine(line: List[Int])
  final case class SlotExit(value: Int, line: List[Symbol], freeSpins: Boolean)
}
