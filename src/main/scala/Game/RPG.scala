package Game

import Game.RPGElements._
import Game.SaveMethods.SaveTailOps
import Game.SlotObjects._
import cats.effect.concurrent.Ref
import cats.effect.IO
import server.Protocol.{Bet, Login}
import RNG._

import scala.annotation.tailrec
import scala.math.pow

object RPG {

  def createNewStage: Stage = Stage(1, 1, Enemy(Mob, 3, 1, 1), Hero(5, 1, Ammunition(0, 0, 0, 0, 0)), 0)

  def createNewRPG(name: Login, rpgProgress: Ref[IO, Map[Login, Stage]]): IO[Map[Login, Stage]] = rpgProgress.modify{
    map => map.get(name) match {
      case None => (map + (name -> createNewStage), map)
      case Some(_) => (map, map)
    }
  }

  def enemyAttack(enemy: Enemy, hero: Hero): Hero = {
    if (hero.ammunition.shield != 0) hero.copy(ammunition = hero.ammunition.copy(shield = hero.ammunition.shield - 1))
    else enemy.enemyType match {
    case _ => generatorRPG match {
        case i if i <= (1 + hero.ammunition.boots) => hero
        case _ => hero.copy(hp = hero.hp - enemy.damage)
      }
    }
  }

  def heroAttack(enemy: Enemy, hero: Hero): Enemy = enemy.enemyType match {
    case _ => generatorRPG match {
      case i if i > enemy.evade => enemy.copy(hp = enemy.hp - hero.damage - hero.ammunition.sword)
      case _ => enemy
    }
  }

  def spawnEnemy(stage: Stage): Enemy = stage.room match {
    case 10 => Enemy(Boss, 10 + (10 * (stage.level - 1)), 1, 1)
    case 5 => Enemy(MiniBoss, 5 + 2 * (stage.level - 1), 1, 2)
    case 11 => Enemy(Mob, 3 + stage.level, 1, 1)
    case _ => Enemy(Mob, 3 + stage.level - 1, 1, 1)
  }

  def getKillReward(bet: Bet, stage: Stage): Map[Stage, Int] = stage.enemy.enemyType match {
    case Boss => Map(Stage(1, stage.level + 1, spawnEnemy(stage), stage.hero, 0) ->
      (bet.amount * pow(5, stage.level) * (1 + 0.1 * stage.hero.ammunition.bag + 0.3 * - stage.enemy.hp)).toInt)
    case MiniBoss => Map(Stage(stage.room + 1, stage.level, enemy = spawnEnemy(stage), stage.hero, turn = 0) ->
      (0.5 * bet.amount * pow(2, stage.level) * (1 + 0.1 * stage.hero.ammunition.bag + 0.2 * - stage.enemy.hp)).toInt)
    case Mob => Map(Stage(stage.room + 1, stage.level, spawnEnemy(stage), stage.hero, 0) ->
      (0.2 * stage.level * bet.amount * (1 + 0.1 * stage.hero.ammunition.bag + 0.1 * - stage.enemy.hp)).toInt)
  }

  def enemyTurn(dmgTurn: Int, stage: Stage): Map[Stage, Int] = stage.turn match {
    case x if x == dmgTurn =>
      val newHero = enemyAttack(stage.enemy, stage.hero)
      newHero.hp + newHero.ammunition.helmet match {
        case i if i <= 0 => Map(createNewStage -> 0)
        case _ => stage.enemy.enemyType match {
          case MiniBoss => Map(stage.copy(room = stage.room + 1, enemy = spawnEnemy(stage), hero = newHero, turn = 0) -> 0)
          case _ => Map(stage.copy(hero = newHero, turn = 0) -> 0)
        }
      }
    case _ => Map(stage.copy(turn = stage.turn + 1) -> 0)
  }

  def fight(NotDoDmgOrDo: Boolean, stage: Stage, bet: Bet): Map[Stage, Int] = {
    val newStage = if (NotDoDmgOrDo) stage
      else stage.copy(enemy = heroAttack(stage.enemy, stage.hero))
    if (newStage.enemy.hp <= 0) getKillReward(bet, newStage)
    else newStage.enemy.enemyType match {
        case Boss => enemyTurn(5, newStage)
        case MiniBoss => enemyTurn(10, newStage)
        case Mob => enemyTurn(7, newStage)
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
  def giveBagEquipment(int: Int, ammunition: Ammunition): Ammunition = if (int <= 0) ammunition
  else ammunition match {
    case Ammunition(helmet, sword, bag, shield, boots) if helmet <= bag => giveBagEquipment(int - 1, Ammunition(helmet + 1, sword, bag, shield, boots))
    case Ammunition(helmet, sword, bag, shield, boots) if sword <= bag => giveBagEquipment(int - 1, Ammunition(helmet, sword + 1, bag, shield, boots))
    case Ammunition(helmet, sword, bag, shield, boots) if shield <= bag => giveBagEquipment(int - 1, Ammunition(helmet, sword, bag, shield + 1, boots))
    case Ammunition(helmet, sword, bag, shield, boots) if boots <= bag => giveBagEquipment(int - 1, Ammunition(helmet, sword, bag, shield, boots + 1))
    case Ammunition(helmet, sword, bag, shield, boots) if bag <= bag => giveBagEquipment(int - 1, Ammunition(helmet, sword, bag + 1, shield, boots))
    case _ => giveBagEquipment(int - 1, ammunition)
  }

  def giveChestEquipment(ammunition: Ammunition): Ammunition = generatorRPG match {
    case i if i <= 2 => ammunition.copy(sword = ammunition.sword + 1)
    case i if i > 2 && i <= 4 => ammunition.copy(helmet = ammunition.helmet + 1)
    case i if i > 4 && i <= 6 => ammunition.copy(ammunition.boots + 1)
    case i if i > 7 && i <= 9 => ammunition.copy(ammunition.bag + 1)
    case _ => ammunition.copy(ammunition.shield + 1)
  }

  def actionGenerator: ActionRPG = generatorRPG match {
    case i if i <= 5 => Damage
    case i if i > 5 && i <= 8 => Heal
    case 9 => Upgrade
    case _ => Death
  }

  @tailrec
  def updateStage(list: List[Element], stage: Stage): Stage =
    list.headOption match {
        case None => stage
        case Some(x) => x match {
          case Sword => updateStage(list.tailSave, stage.copy(enemy = heroAttack(stage.enemy, stage.hero)))
          case Bag => updateStage(list.tailSave, stage.copy(hero = stage.hero.copy(ammunition = giveBagEquipment(1, stage.hero.ammunition))))
          case Chest => updateStage(list.tailSave, stage.copy(hero = stage.hero.copy(ammunition = giveChestEquipment(stage.hero.ammunition))))
          case Action => actionGenerator match {
            case Damage => updateStage(list.tailSave, stage.copy(enemy = heroAttack(stage.enemy, stage.hero.copy(damage = stage.hero.damage + 10))))
            case Heal => updateStage(list.tailSave, stage.copy(hero = stage.hero.copy(5)))
            case Upgrade => updateStage(list.tailSave, stage.copy(hero = stage.hero.copy(stage.hero.hp + 1, stage.hero.damage + 1, giveBagEquipment(5, stage.hero.ammunition))))
            case Death => updateStage(list.tailSave, createNewStage)
            case _ => stage
          }
          case NoElement => stage
          case Jackpot => Stage(11, stage.level, Enemy(Boss, 0, 0, 0), stage.hero, stage.turn)
          case _ => stage
        }
      }


  def playRPG(list: List[Element], login: Login,  bet: Bet, rpgProgress: Ref[IO, Map[Login, Stage]]): IO[Map[Stage, Int]] = for {
    stage <- getStage(login, rpgProgress)
    updatedStage = updateStage(list, stage)
    map = fight(list.isEmpty, updatedStage, bet)
    newStage = map.keys.toList.headOption match {
              case None => stage
              case Some(value) => value
            }
    _ <- updateProgress(login, newStage, rpgProgress)
  } yield map
}
