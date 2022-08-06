package game.models

object RPGElements {

  sealed trait EnemyType

  final case object Boss extends EnemyType

  final case object MiniBoss extends EnemyType

  final case object Mob extends EnemyType

  final case class Enemy(enemyType: EnemyType, hp: Int, damage: Int, evade: Int)

  final case class Ammunition(helmet: Int, sword: Int, bag: Int, shield: Int, boots: Int)

  final case class Hero(hp: Int, damage: Int, ammunition: Ammunition)

  final case class Stage(room: Int, level: Int, enemy: Enemy, hero: Hero, turn: Int)

  sealed trait ActionRPG

  final case object Damage extends ActionRPG

  final case object Heal extends ActionRPG

  final case object Upgrade extends ActionRPG

  final case object Death extends ActionRPG

  final case object NoAction extends ActionRPG
}
