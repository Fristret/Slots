package game

import cats.{Applicative, Monad}
import cats.implicits._
import cats.effect.concurrent.Ref
import game.models.RPGElements._
import game.models.SlotObjects._
import game.utils.SaveMethods.SaveTailOps
import server.models.Protocol.{Bet, Login}
import game.utils.RNG

import scala.annotation.tailrec
import scala.math.pow

trait RPG2[F[_]]{
  def createNewRPG: F[Map[Login, Stage]]
  def randomRPG: F[Int]
  def enemyAttack(enemy: Enemy, hero: Hero): F[Hero]
  def heroAttack(enemy: Enemy, hero: Hero): F[Enemy]
  def spawnEnemy(stage: Stage): Enemy
  def getKillReward(stage: Stage): Map[Stage, Int]
  def enemyTurn(dmgTurn: Int, stage: Stage): F[Map[Stage, Int]]
  def fight(NotDoDmgOrDo: Boolean, stage: Stage): F[Map[Stage, Int]]
  def updateProgress(stage: Stage): F[Unit]
  def getStage: F[Stage]
  def giveBagEquipment(int: Int, ammunition: Ammunition): Ammunition
  def giveChestEquipment(ammunition: Ammunition): F[Ammunition]
  def actionGenerator: F[ActionRPG]
  def updateStage(listElement: Element, stage: Stage): F[Stage]
  def playRPG: F[Map[Stage, Int]]
}


object RPG2 {

  def createNewStage: Stage = Stage(1, 1, Enemy(Mob, 3, 1, 1), Hero(5, 1, Ammunition(0, 0, 0, 0, 0)), 0)

  def apply[F[_] : Applicative : Monad](listElement: List[Element], login: Login, bet: Bet, rpgProgress: Ref[F, Map[Login, Stage]]): RPG2[F] = new RPG2[F] {

    def createNewRPG: F[Map[Login, Stage]] = rpgProgress.modify{
      map => map.get(login) match {
        case None => (map + (login -> createNewStage), map)
        case Some(_) => (map, map)
      }
    }

    def randomRPG: F[Int] = for{
      _ <- "23".pure[F]
      rng = RNG.apply[F]
      int <- rng.generatorRPG
    } yield int

    def enemyAttack(enemy: Enemy, hero: Hero): F[Hero] = {
      if (hero.ammunition.shield != 0) hero.copy(ammunition = hero.ammunition.copy(shield = hero.ammunition.shield - 1)).pure[F]
      else enemy.enemyType match {
        case _ => randomRPG.map{
          case i if i <= (1 + hero.ammunition.boots) => hero
          case _ => hero.copy (hp = hero.hp - enemy.damage)
        }
      }
    }

    def heroAttack(enemy: Enemy, hero: Hero): F[Enemy] = enemy.enemyType match {
      case _ => randomRPG.map {
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

    def getKillReward(stage: Stage): Map[Stage, Int] = stage.enemy.enemyType match {
      case Boss => Map(Stage(1, stage.level + 1, spawnEnemy(stage), stage.hero, 0) ->
        (bet.amount * pow(5, stage.level) * (1 + 0.1 * stage.hero.ammunition.bag + 0.3 * - stage.enemy.hp)).toInt)
      case MiniBoss => Map(Stage(stage.room + 1, stage.level, enemy = spawnEnemy(stage), stage.hero, turn = 0) ->
        (0.5 * bet.amount * pow(2, stage.level) * (1 + 0.1 * stage.hero.ammunition.bag + 0.2 * - stage.enemy.hp)).toInt)
      case Mob => Map(Stage(stage.room + 1, stage.level, spawnEnemy(stage), stage.hero, 0) ->
        (0.2 * stage.level * bet.amount * (1 + 0.1 * stage.hero.ammunition.bag + 0.1 * - stage.enemy.hp)).toInt)
    }

    def enemyTurn(dmgTurn: Int, stage: Stage): F[Map[Stage, Int]] = stage.turn match {
      case x if x == dmgTurn => enemyAttack(stage.enemy, stage.hero).map(newHero =>
        newHero.hp + newHero.ammunition.helmet match {
          case i if i <= 0 => Map(createNewStage -> 0)
          case _ => stage.enemy.enemyType match {
            case MiniBoss => Map(stage.copy(room = stage.room + 1, enemy = spawnEnemy(stage), hero = newHero, turn = 0) -> 0)
            case _ => Map(stage.copy(hero = newHero, turn = 0) -> 0)
          }
        })
      case _ => Map(stage.copy(turn = stage.turn + 1) -> 0).pure[F]
    }

    def fight(NotDoDmgOrDo: Boolean, stage: Stage): F[Map[Stage, Int]] = for {
        newEnemy <- heroAttack(stage.enemy, stage.hero)
        newStage = if (NotDoDmgOrDo) stage else stage.copy(enemy = newEnemy)
        res <- if (newStage.enemy.hp <= 0) getKillReward(newStage).pure[F]
          else newStage.enemy.enemyType match {
            case Boss => enemyTurn(5, newStage)
            case MiniBoss => enemyTurn(10, newStage)
            case Mob => enemyTurn(7, newStage)
          }
    } yield res

    def updateProgress(stage: Stage): F[Unit] = rpgProgress.update(
      map => map.get(login) match {
        case None => map ++ Map(login -> stage)
        case Some(_) => map ++ Map(login -> stage)
      }
    )

    def getStage: F[Stage] = rpgProgress.get.flatMap(map => map.get(login) match {
      case None => createNewStage.pure[F]
      case Some(value) => value.pure[F]
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

    def giveChestEquipment(ammunition: Ammunition): F[Ammunition] = randomRPG.map{
      case i if i <= 2 => ammunition.copy(sword = ammunition.sword + 1)
      case i if i > 2 && i <= 4 => ammunition.copy(helmet = ammunition.helmet + 1)
      case i if i > 4 && i <= 6 => ammunition.copy(ammunition.boots + 1)
      case i if i > 7 && i <= 9 => ammunition.copy(ammunition.bag + 1)
      case _ => ammunition.copy(ammunition.shield + 1)
    }

    def actionGenerator: F[ActionRPG] = randomRPG.map{
      case i if i <= 5 => Damage
      case i if i > 5 && i <= 8 => Heal
      case 9 => Upgrade
      case _ => Death
    }

    def updateStage(element: Element, stage: Stage): F[Stage] =
        element match {
          case Sword => heroAttack(stage.enemy, stage.hero).map(newEnemy => stage.copy(enemy = newEnemy))
          case Bag => stage.copy(hero = stage.hero.copy(ammunition = giveBagEquipment(1, stage.hero.ammunition))).pure[F]
          case Chest => giveChestEquipment(stage.hero.ammunition).map(ammunition => stage.copy(hero = stage.hero.copy(ammunition = ammunition)))
          case Action => actionGenerator.flatMap {
            case Damage => heroAttack(stage.enemy, stage.hero.copy(damage = stage.hero.damage + 10)).map(enemy => stage.copy(enemy = enemy))
            case Heal => stage.copy(hero = stage.hero.copy(5)).pure[F]
            case Upgrade => stage.copy(hero = stage.hero.copy(stage.hero.hp + 1, stage.hero.damage + 1, giveBagEquipment(5, stage.hero.ammunition))).pure[F]
            case Death => createNewStage.pure[F]
            case _ => stage.pure[F]
          }
          case NoElement => stage.pure[F]
          case Jackpot => Stage(11, stage.level, Enemy(Boss, 0, 0, 0), stage.hero, stage.turn).pure[F]
          case _ => stage.pure[F]
        }

    def rec(list: List[Element], stage: Stage): F[Stage] = if (list.isEmpty) stage.pure[F]
    else {
      val element = list.headOption match {
        case Some(x) => x
        case None => NoElement
      }
      for {
        newStage <- updateStage(element, stage)
        res <- rec(list.tailSave, newStage)
      } yield res
    }


    def playRPG: F[Map[Stage, Int]] = for {
      stage <- getStage
      updatedStage <- rec(listElement, stage)
      map <- fight(listElement.isEmpty, updatedStage)
      newStage = map.keys.toList.headOption match {
        case None => stage
        case Some(value) => value
      }
      _ <- updateProgress(newStage)
    } yield map
  }
}
