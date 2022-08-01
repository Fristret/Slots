package Game

import Game.RPGElements._
import Game.SlotObjects._
import cats.effect.concurrent.Ref
import cats.effect.IO
import server.Protocol.{Bet, Login}
import RNG._

import scala.annotation.tailrec
import scala.math.pow

object RPG {

  def createNewStage: Stage = Stage(1, 1, Mob(3, 1, 1), Hero(5, 1, Ammunition(0, 0, 0, 0, 0)), 0)

  def createNewRPG(name: Login, rpgProgress: Ref[IO, Map[Login, Stage]]): IO[Map[Login, Stage]] = rpgProgress.modify{
    map => map.get(name) match {
      case None => (map + (name -> createNewStage), map)
      case Some(_) => (map, map)
    }
  }

  def enemyAttack(enemy: Enemy, hero: Hero): Hero = {
    if (hero.ammunition.shield != 0) Hero(hero.hp, hero.damage, Ammunition(hero.ammunition.helmet, hero.ammunition.sword, hero.ammunition.bag, hero.ammunition.shield - 1, hero.ammunition.boots))
    else enemy match {
    case boss: Boss =>  generatorRPG match {
        case i if i <= (1 + hero.ammunition.boots) => hero
        case _ => Hero(hero.hp + hero.ammunition.helmet - boss.damage, hero.damage, hero.ammunition)
    }
    case miniBoss: MiniBoss => generatorRPG match {
        case i if i <= (1 + hero.ammunition.boots) => hero
        case _ => Hero(hero.hp + hero.ammunition.helmet - miniBoss.damage, hero.damage, hero.ammunition)
    }
    case mob: Mob => generatorRPG match {
        case i if i <= (1 + hero.ammunition.boots) => hero
        case _ => Hero(hero.hp + hero.ammunition.helmet  - mob.damage, hero.damage, hero.ammunition)
      }
  }
  }

  def heroAttack(enemy: Enemy, hero: Hero): Enemy = enemy match {
    case boss: Boss => generatorRPG match {
      case i if i > boss.evade => Boss(boss.hp - hero.damage - hero.ammunition.sword, boss.damage, boss.evade)
      case _ => boss
    }
    case miniBoss: MiniBoss => generatorRPG match {
      case i if i > miniBoss.evade => MiniBoss(miniBoss.hp - hero.damage - hero.ammunition.sword, miniBoss.damage, miniBoss.evade)
      case _ => miniBoss
    }
    case mob: Mob => generatorRPG match {
      case i if i >= mob.evade => Mob(mob.hp - hero.damage - hero.ammunition.sword, mob.damage, mob.evade)
      case _ => mob
    }
  }

  def fight(doDmgOrNot: Boolean, stage: Stage, bet: Bet): Map[Stage, Int] =
    (if (doDmgOrNot) stage.enemy
    else heroAttack(stage.enemy, stage.hero)) match {
      case boss: Boss => if (boss.hp <= 0) Map(Stage(1, stage.level + 1, Mob(3 + stage.level, 1, 1), stage.hero, 0) -> (bet.amount * pow(10, stage.level) + bet.amount * pow(10, stage.level) * stage.hero.ammunition.bag).toInt)
        else stage.turn match {
        case 5 =>
          val newHero = enemyAttack(stage.enemy, stage.hero)
          newHero.hp match {
            case i if i <= 0 => Map(createNewStage -> 0)
            case _ => Map(Stage(stage.room, stage.level, boss, newHero, 0) -> 0)
          }
        case _ => Map(Stage(stage.room, stage.level, boss, stage.hero, stage.turn + 1) -> 0)
      }

      case miniBoss: MiniBoss => if (miniBoss.hp <= 0) Map(Stage(stage.room + 1, stage.level, Mob(3 + stage.level, 1, 1), stage.hero, 0) -> (bet.amount * pow(5, stage.level) + bet.amount * pow(5, stage.level) * stage.hero.ammunition.bag).toInt)
      else stage.turn match {
        case 10 =>
          val newHero = enemyAttack(stage.enemy, stage.hero)
          newHero.hp match {
            case i if i <= 0 => Map(createNewStage -> 0)
            case _ => Map(Stage(stage.room + 1, stage.level, Mob(3 + stage.level, 1, 1), newHero, 0) -> 0)
          }
        case _ => Map(Stage(stage.room, stage.level, miniBoss, stage.hero, stage.turn + 1) -> 0)
      }

      case mob: Mob => if (mob.hp <= 0) Map((stage.room match {
        case 10 => Stage(stage.room + 1, stage.level, Boss(10 + (10 * stage.level - 1), 1, 2), stage.hero, 0)
        case 5 => Stage(stage.room + 1, stage.level, MiniBoss(7 + stage.level - 1, 1, 3), stage.hero, 0)
        case _ => Stage(stage.room + 1, stage.level, Mob(3 + stage.level - 1, 1, 1), stage.hero, 0)
      }) -> (bet.amount * pow(2, stage.level) + bet.amount * pow(2, stage.level) * stage.hero.ammunition.bag).toInt)
      else stage.turn match {
        case 7 =>
          val newHero = enemyAttack(stage.enemy, stage.hero)
          newHero.hp match {
            case i if i <= 0 => Map(createNewStage -> 0)
            case _ => Map(Stage(stage.room, stage.level, mob, newHero, 0) -> 0)
          }
        case _ => Map(Stage(stage.room, stage.level, mob, stage.hero, stage.turn + 1) -> 0)
      }
    }

  def updateProgress(login: Login, stage: Stage, rpgProgress: Ref[IO, Map[Login, Stage]]): IO[Unit] = rpgProgress.update(
    map => map.get(login) match {
      case None => map ++ Map(login -> stage)
      case Some(_) => map ++ Map(login -> stage)
    }
  )

  def getStage(login: Login, rpgProgress: Ref[IO, Map[Login, Stage]]): IO[Stage] = rpgProgress.get.flatMap(map => map.get(login) match {
    case None => IO.raiseError(new IllegalAccessError("Hero didn't exist"))
    case Some(value) => IO(value)
  }
  )

  @tailrec
  def giveBagEquipment(int: Int, ammunition: Ammunition): Ammunition = if(int == 0) ammunition
  else ammunition match {
    case Ammunition(helmet, sword, bag, shield, boots) if helmet < bag => giveBagEquipment(int - 1, Ammunition(helmet + 1, sword, bag, shield, boots))
    case Ammunition(helmet, sword, bag, shield, boots)  if sword < bag => giveBagEquipment(int - 1, Ammunition(helmet, sword + 1, bag, shield, boots))
    case Ammunition(helmet, sword, bag, shield, boots)  if shield < bag => giveBagEquipment(int - 1, Ammunition(helmet, sword, bag, shield + 1, boots))
    case Ammunition(helmet, sword, bag, shield, boots)  if boots < bag => giveBagEquipment(int - 1, Ammunition(helmet, sword, bag, shield, boots + 1))
    case Ammunition(helmet, sword, bag, shield, boots)  if bag < bag => giveBagEquipment(int - 1, Ammunition(helmet, sword, bag + 1, shield, boots))
    case _ => giveBagEquipment(int - 1, ammunition)
  }

  def giveChestEquipment(ammunition: Ammunition): Ammunition = generatorRPG match {
    case i if i <= 2 => Ammunition(
          ammunition.helmet,
          ammunition.sword + 1,
          ammunition.bag,
          ammunition.shield,
          ammunition.boots)
    case i if i > 2 && i <= 4 => Ammunition(
          ammunition.helmet + 1,
          ammunition.sword,
          ammunition.bag,
          ammunition.shield,
          ammunition.boots)
    case i if i > 4 && i <= 6 => Ammunition(
          ammunition.helmet,
          ammunition.sword,
          ammunition.bag,
          ammunition.shield,
          ammunition.boots + 1)
    case i if i > 7 && i <= 9 => Ammunition(
          ammunition.helmet,
          ammunition.sword,
          ammunition.bag + 1,
          ammunition.shield,
          ammunition.boots)
    case _ => Ammunition(
          ammunition.helmet,
          ammunition.sword,
          ammunition.bag,
          ammunition.shield + 1,
          ammunition.boots)
  }

  def action(stage: Stage): Stage = generatorRPG match {
    case i if i <= 5 => Stage(stage.room, stage.level, heroAttack(stage.enemy, Hero(stage.hero.hp, 10, stage.hero.ammunition)), stage.hero, stage.turn) //dmg enemy
    case i if i > 5 && i <= 8 => Stage(stage.room, stage.level, stage.enemy, Hero(5, stage.hero.damage, stage.hero.ammunition), stage.turn) //Heal
    case 9 => Stage(stage.room, stage.level, stage.enemy, Hero(stage.hero.hp + 1, stage.hero.damage + 1, giveBagEquipment(5, stage.hero.ammunition)), stage.turn) //5 times Bag
    case _ => createNewStage //Death
  }

  def updateStage(list: List[Element], stage: Stage): Stage =
    list.headOption match {
        case None => stage
        case Some(x) => x match {
          case Sword => Stage(stage.room, stage.level, heroAttack(stage.enemy, stage.hero), stage.hero, stage.turn)
          case Bag => Stage(stage.room, stage.level,stage.enemy, Hero(stage.hero.hp, stage.hero.damage, giveBagEquipment(1, stage.hero.ammunition)), stage.turn)
          case Chest => Stage(stage.room, stage.level,stage.enemy, Hero(stage.hero.hp, stage.hero.damage, giveChestEquipment(stage.hero.ammunition)), stage.turn)
          case Action => action(stage)
          case NoElement => stage
          case Jackpot => Stage(11, stage.level, Boss(0, 0, 0), stage.hero, stage.turn)
          case _ => stage
        }
      }


  def playRPG(list: List[Element], login: Login,  bet: Bet, rpgProgress: Ref[IO, Map[Login, Stage]]): IO[Map[Stage, Int]] = for {
    stage <- getStage(login, rpgProgress)
    map = fight(list.isEmpty, updateStage(list, stage), bet)
    newStage = map.keys.toList.headOption match {
              case None => stage
              case Some(value) => value
            }
    _ <- updateProgress(login, newStage, rpgProgress)
  } yield map
}
