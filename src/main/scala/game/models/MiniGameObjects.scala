package game.models

object MiniGameObjects {
  sealed trait MiniGameUnit
  final case object Water extends MiniGameUnit
  final case object Air extends MiniGameUnit
  final case object Leaf extends MiniGameUnit
  final case object Fire extends MiniGameUnit
  final case object ZaWarudo extends MiniGameUnit
  final case object Weak extends MiniGameUnit
}
