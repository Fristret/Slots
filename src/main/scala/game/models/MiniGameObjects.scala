package game.models

import game.models.SlotObjects.Symbol

object MiniGameObjects {
  sealed trait MiniGameUnit
  final case object Water extends MiniGameUnit
  final case object Air extends MiniGameUnit
  final case object Leaf extends MiniGameUnit
  final case object Fire extends MiniGameUnit
  final case object ZaWarudo extends MiniGameUnit

  final case class MiniGameOutput(spiritHero: MiniGameUnit, spiritEnemy: MiniGameUnit, result: List[Symbol])
}
