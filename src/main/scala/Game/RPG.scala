package Game

import Game.RPGElements._
import Game.SlotObjects._
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import server.Protocol.{Bet, Login}

import RNG._

object RPG {

  def createNewStage: Stage = Stage(1, 1, Mob(3, 0, 1), Hero(5, 1, Ammunition(0, 0, 0, 0, 0)), 0)

  def createNewRPG(name: Login, rpgProgress: Ref[IO, Map[Login, Stage]]): IO[Map[Login, Stage]] = rpgProgress.modify{
    map => map.get(name) match {
      case None => (map + (name -> createNewStage), map)
      case Some(x) => (map, map)
    }
  }

  def enemyAttack(enemy: Enemy, hero: Hero): Hero = enemy match {
    case boss: Boss =>  generatorRPG match {
        case i if i <= (1 + hero.ammunition.boots) => hero
        case _ => Hero(hero.hp - boss.damage, hero.damage, hero.ammunition)
    }
    case miniBoss: MiniBoss => generatorRPG match {
        case i if i <= (1 + hero.ammunition.boots) => hero
        case _ => Hero(hero.hp - miniBoss.damage, hero.damage, hero.ammunition)
    }
    case mob: Mob => generatorRPG match {
        case i if i <= (1 + hero.ammunition.boots) => hero
        case _ => Hero(hero.hp - mob.damage, hero.damage, hero.ammunition)
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

  def fight(stage: Stage): Stage =
    heroAttack(stage.enemy, stage.hero) match {
      case boss: Boss => if (boss.hp <= 0) Stage(0, stage.level + 1, Mob(3 + stage.level, 1, 1), stage.hero, 0)
        else stage.turn match {
        case 5 =>
          val newHero = enemyAttack(stage.enemy, stage.hero)
          newHero.hp match {
            case i if i <= 0 => createNewStage
            case _ => Stage(stage.room, stage.level, boss, newHero, 0)
          }
        case _ => Stage(stage.room, stage.level, boss, stage.hero, stage.turn + 1)
      }

      case miniBoss: MiniBoss => if (miniBoss.hp <= 0) Stage(stage.room + 1, stage.level, Mob(3 + stage.level, 1, 1), stage.hero, 0)
      else stage.turn match {
        case 10 =>
          val newHero = enemyAttack(stage.enemy, stage.hero)
          newHero.hp match {
            case i if i <= 0 => createNewStage
            case _ => Stage(stage.room + 1, stage.level, Mob(3 + stage.level, 1, 1), newHero, 0)
          }
        case _ => Stage(stage.room, stage.level, miniBoss, stage.hero, stage.turn + 1)
      }

      case mob: Mob => if (mob.hp <= 0) stage.room match {
        case 10 => Stage(stage.room + 1, stage.level, Boss(10 + (10 * stage.level - 1), 1, 2), stage.hero, 0)
        case 5 => Stage(stage.room + 1, stage.level, MiniBoss(7 + stage.level - 1, 1, 3), stage.hero, 0)
        case _ => Stage(stage.room + 1, stage.level, Mob(3 + stage.level, 1, 1), stage.hero, 0)
      }
      else stage.turn match {
        case 7 =>
          val newHero = enemyAttack(stage.enemy, stage.hero)
          newHero.hp match {
            case i if i <= 0 => createNewStage
            case _ => Stage(stage.room, stage.level, mob, newHero, 0)
          }
        case _ => Stage(stage.room, stage.level, mob, stage.hero, stage.turn + 1)
      }
    }

  def updateProgress(login: Login, stage: Stage, rpgProgress: Ref[IO, Map[Login, Stage]]) = rpgProgress.modify(
    map => map.get(login) match {
      case None => (map + (login -> stage), map)
      case Some(value) => (map + (login -> stage), map)
    }
  )

  def getStage(login: Login, rpgProgress: Ref[IO, Map[Login, Stage]]) = rpgProgress.get.flatMap(map => map.get(login) match {
    case None => IO.raiseError(new IllegalAccessError("Hero didn't exist"))
    case Some(value) => IO(value)
  }
  )


  def playRPG(list: List[Element], login: Login,  bet: Bet, rpgProgress: Ref[IO, Map[Login, Stage]]): Stage = {
    val stage = getStage(login, rpgProgress).unsafeRunSync()
    val newStage = fight(stage)
    updateProgress(login, newStage, rpgProgress)
    newStage
  }


//  - <- list.headOption match {
//    case None => 0
//    case Some(x) => x match {
//      case Sword => ???
//      case Bag => ???
//      case Chest => ???
//      case Action => ???
//      case NoElement => 0
//      case Jackpot =>
//      case _ =>
//    }
//  }
//  } yield ()


}
