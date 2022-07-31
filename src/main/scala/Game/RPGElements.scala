package Game

object RPGElements {

  trait Enemy
  case class Boss(hp: Int,  damage: Int, evade: Int) extends Enemy
  case class MiniBoss(hp: Int,  damage: Int, evade: Int) extends Enemy
  case class Mob(hp: Int,  damage: Int, evade: Int) extends Enemy

  trait Character
  case class Ammunition(helmet: Int, sword: Int, bag: Int, shield: Int, boots: Int) extends Character
  case class Hero(hp: Int, damage: Int, ammunition: Ammunition) extends Character

  trait RPGEnvironment
  case class Stage(room: Int, level: Int, enemy: Enemy, hero: Hero, turn: Int) extends RPGEnvironment

  trait Equipment
  case object HelmetRPG extends  Equipment
  case object SwordRPG extends Equipment
  case object BagRPG extends Equipment
  case object ShieldRPG extends Equipment
  case object BootsRPG extends Equipment

}
