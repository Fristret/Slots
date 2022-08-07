package GameTests

import game.RPG2.createNewStage
import game.models.RPGElements._
import game.models.SlotObjects._
import cats.effect.IO
import cats.effect.concurrent.Ref
import game.RPG2
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import server.models.Protocol._

import scala.math.pow

class RPGTests extends AnyFreeSpec with Matchers{

  val playerLogin: Login = Login("masana")
  val playerLogin2: Login = Login("masana234")
  val playerLogin3: Login = Login("masana234")
  val customStage: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1)
  val emptyRef: IO[Ref[IO, Map[Login, Stage]]] = Ref[IO].of(Map.empty[Login, Stage])
  val notEmptyRef: IO[Ref[IO, Map[Login, Stage]]] = Ref[IO].of(Map(playerLogin -> createNewStage, playerLogin2 -> customStage))

  val pgGame: IO[RPG2[IO]] = notEmptyRef.map(ref => RPG2(List(Action, Sword, Point5, Point5), playerLogin, Bet(200), ref))
  val pgGameCreate: IO[RPG2[IO]] = emptyRef.map(ref => RPG2(List(Action, Sword, Point5, Point5), playerLogin, Bet(200), ref))

  "creating new RPG Game"  - {
    "create" in {
      val map = Map(playerLogin -> createNewStage)
      val action = for {
        rpg <- pgGameCreate
        value <- rpg.createNewRPG
      } yield value
      action.unsafeRunSync() should be (map)
    }

    "return exists" in {
      val action = for {
        pgGame <- pgGame
        value <- pgGame.createNewRPG
      } yield value.get(playerLogin2)
      action.unsafeRunSync() should be (Some(customStage))
    }
  }

  "enemy attack" - {
    "success damage, evade damage and block damage" in {
      val stageDmg: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, -1)), 1)
      val stageEv: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 10)), 1)
      val stageBl: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 1, 0)), 1)
      val action = for {
        pgGame <- pgGame
        heroDmg <- pgGame.enemyAttack(stageDmg.enemy, stageDmg.hero)
        heroEv <- pgGame.enemyAttack(stageEv.enemy, stageEv.hero)
        heroBl <- pgGame.enemyAttack(stageBl.enemy, stageBl.hero)
      } yield (heroDmg, heroEv, heroBl)
      action.unsafeRunSync()._1 should be(Hero(1, 1, Ammunition(0, 2, 0, 0, -1)))
      action.unsafeRunSync()._2 should be (Hero(2, 1, Ammunition(0, 2, 0, 0, 10)))
      action.unsafeRunSync()._3 should be(Hero(2, 1, Ammunition(0, 2, 0, 0, 0)))
    }
  }

  "hero attack" - {
    "success damage, success evade" in {
      val stageDmg: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1)
      val stageEv: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 10), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1)
      val action = for {
        pgGame <- pgGame
        enemyDmg <- pgGame.heroAttack(stageDmg.enemy, stageDmg.hero)
        enemyEv <- pgGame.heroAttack(stageEv.enemy, stageEv.hero)
      } yield (enemyDmg, enemyEv)
      action.unsafeRunSync()._1 should be (Enemy(Mob, -1, 1, 0))
      action.unsafeRunSync()._2 should be (Enemy(Mob, 2, 1, 10))
    }
  }

  "fight" - {
    "No dmg" in {
      val stage: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1)
      val action = for {
        pgGame <- pgGame
        map <- pgGame.fight(NotDoDmgOrDo = true, stage)
      } yield map
        action.unsafeRunSync() should be (Map(Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 2) -> 0))


    }
    "Kill enemies" in {
      val stage1: Stage = Stage(1, 1, Enemy(Mob, 1, 1, 0), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 1)
      val stage11: Stage = Stage(5, 1, Enemy(Mob, 1, 1, 0), Hero(2, 1, Ammunition(0, 10, 2, 0, 0)), 1)
      val stage2: Stage = Stage(6, 3, Enemy(MiniBoss, 1, 1, 0), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 1)
      val stage3: Stage = Stage(11, 3, Enemy(Boss, 1, 1, 0), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 1)
      val action = for{
        pgGame <- pgGame
        map1 <- pgGame.fight(NotDoDmgOrDo = false, stage1)
        map11 <- pgGame.fight(NotDoDmgOrDo = false, stage11)
        map2 <- pgGame.fight(NotDoDmgOrDo = false, stage2)
        map3 <- pgGame.fight(NotDoDmgOrDo = false, stage3)
      } yield (map1, map11, map2, map3)
      action.unsafeRunSync()._1 should be (Map(Stage(2, 1, Enemy(Mob, 3, 1, 1), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 0) -> (0.2 * stage1.level * 200).toInt))
      action.unsafeRunSync()._2 should be (Map(Stage(6, 1, Enemy(MiniBoss, 5, 1, 2), Hero(2, 1, Ammunition(0, 10, 2, 0, 0)), 0) -> (0.2 * 200 * stage11.level * (1 + 0.1 * stage11.hero.ammunition.bag + 1)).toInt))
      action.unsafeRunSync()._3 should be (Map(Stage(7, 3,Enemy(Mob, 5, 1, 1), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 0) -> 0.5 * 200 * pow(2, stage2.level)))
      action.unsafeRunSync()._4 should be (Map(Stage(1, 4, Enemy(Mob, 6, 1, 1), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 0) -> 200 * pow(5, stage3.level)))
    }
    "Hero Death" in {
      val stage: Stage = Stage(1, 1, Enemy(Mob, 2, 1, 0), Hero(1, 0, Ammunition(0, 0, 0, 0, -1)), 7)
      val action = for {
        pgGame <- pgGame
        map <- pgGame.fight(NotDoDmgOrDo = true, stage)
      } yield map
       action.unsafeRunSync() should be (Map(createNewStage -> 0))
    }
  }

  "give bag" - {
    "5 times" in {
      val ammunition = Ammunition(0, 0, 0, 0, 0)
      val action = for {
        pgGame <- pgGame
        ammunitionNew = pgGame.giveBagEquipment(5, ammunition)
      } yield ammunitionNew
      action.unsafeRunSync() should be (Ammunition(1, 1, 1, 1, 1))
    }
  }

  "update stage" - {
    "sword" in {
      val element: Element = Sword
      val action = for{
        pgGame <- pgGame
        stage <- pgGame.updateStage(element, customStage)
      } yield stage
      action.unsafeRunSync() should be (Stage(2, 3, Enemy(Mob, -1, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1))
    }
  }
}
