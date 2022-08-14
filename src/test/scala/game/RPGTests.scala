package game

import game.RPG.createNewStage
import game.models.RPGElements._
import game.models.SlotObjects._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.effect.testing.scalatest.AsyncIOSpec
import fs2.concurrent.Topic
import game.models.MiniGameObjects.Leaf
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import server.models.Protocol._

import scala.math.pow

class RPGTests extends AsyncFreeSpec with Matchers with AsyncIOSpec {

  val playerLogin: Login = Login("masana")
  val playerLogin2: Login = Login("masana234")
  val playerLogin3: Login = Login("masana234")
  val topicClient: IO[Topic[IO, String]] = Topic[IO, String]("")
  val customStage: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1)
  val emptyRef: IO[Ref[IO, Map[Login, Stage]]] = Ref[IO].of(Map.empty[Login, Stage])
  val notEmptyRef: IO[Ref[IO, Map[Login, Stage]]] = Ref[IO].of(Map(playerLogin -> createNewStage, playerLogin2 -> customStage))

  val pgGame: IO[RPG[IO]] = for {
    ref <- notEmptyRef
    topic <- topicClient
  } yield RPG(List(Action, Sword, Bag, Point5), playerLogin, Bet(200), topic, ref, Leaf)
  val pgGameCreate: IO[RPG[IO]] = for {
    ref <- emptyRef
    topic <- topicClient
  } yield RPG(List(Action, Sword, Bag, Point5), playerLogin, Bet(200), topic, ref, Leaf)

  "RPG should" - {
    "enemy attack: success damage, evade damage and block damage" in {
      val stageDmg: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, -1)), 1)
      val stageEv: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 10)), 1)
      val stageBl: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 1, 0)), 1)
      val action = for {
        pgGame <- pgGame
        heroDmg <- pgGame.enemyAttack(stageDmg.enemy, stageDmg.hero)
        heroEv <- pgGame.enemyAttack(stageEv.enemy, stageEv.hero)
        heroBl <- pgGame.enemyAttack(stageBl.enemy, stageBl.hero)
      } yield (heroDmg, heroEv, heroBl)
      action.asserting(_._1 shouldBe Hero(1, 1, Ammunition(0, 2, 0, 0, -1)))
      action.asserting(_._2 shouldBe Hero(2, 1, Ammunition(0, 2, 0, 0, 10)))
      action.asserting(_._3 shouldBe Hero(2, 1, Ammunition(0, 2, 0, 0, 0)))
    }

    "hero attack: success damage, success evade" in {
      val stageDmg: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1)
      val stageEv: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 10), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1)
      val action = for {
        pgGame <- pgGame
        enemyDmg <- pgGame.heroAttack(stageDmg.enemy, stageDmg.hero)
        enemyEv <- pgGame.heroAttack(stageEv.enemy, stageEv.hero)
      } yield (enemyDmg, enemyEv)
      action.asserting(_._1 shouldBe Enemy(Mob, -1, 1, 0))
      action.asserting(_._2 shouldBe Enemy(Mob, 2, 1, 10))
    }

    "do fight with No dmg" in {
      val stage: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1)
      val action = for {
        pgGame <- pgGame
        map <- pgGame.fight(NotDoDmgOrDo = true, stage)
      } yield map
      action.asserting(_ shouldBe Map(Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 2) -> 0))
    }

    "do fight with hero killing enemies" in {
      val stage1: Stage = Stage(1, 1, Enemy(Mob, 1, 1, 0), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 1)
      val stage11: Stage = Stage(5, 1, Enemy(Mob, 1, 1, 0), Hero(2, 1, Ammunition(0, 10, 2, 0, 0)), 1)
      val stage2: Stage = Stage(6, 3, Enemy(MiniBoss, 1, 1, 0), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 1)
      val stage3: Stage = Stage(11, 3, Enemy(Boss, 1, 1, 0), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 1)
      val action = for {
        pgGame <- pgGame
        map1 <- pgGame.fight(NotDoDmgOrDo = false, stage1)
        map11 <- pgGame.fight(NotDoDmgOrDo = false, stage11)
        map2 <- pgGame.fight(NotDoDmgOrDo = false, stage2)
        map3 <- pgGame.fight(NotDoDmgOrDo = false, stage3)
      } yield (map1, map11, map2, map3)
      action.asserting(_._1 shouldBe Map(Stage(2, 1, Enemy(Mob, 3, 1, 1), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 0) -> (0.2 * stage1.level * 200).toInt))
      action.asserting(_._2 shouldBe Map(Stage(6, 1, Enemy(MiniBoss, 5, 1, 2), Hero(2, 1, Ammunition(0, 10, 2, 0, 0)), 0) -> (0.2 * 200 * stage11.level * (1 + 0.1 * stage11.hero.ammunition.bag + 1)).toInt))
      action.asserting(_._3 shouldBe Map(Stage(7, 3, Enemy(Mob, 5, 1, 1), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 0) -> 0.5 * 200 * pow(2, stage2.level)))
      action.asserting(_._4 shouldBe Map(Stage(1, 4, Enemy(Mob, 6, 1, 1), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 0) -> 200 * pow(5, stage3.level)))
    }

    "kill hero" in {
      val stage: Stage = Stage(1, 1, Enemy(Mob, 2, 1, 0), Hero(1, 0, Ammunition(0, 0, 0, 0, -1)), 7)
      val action = for {
        pgGame <- pgGame
        map <- pgGame.fight(NotDoDmgOrDo = true, stage)
      } yield map
      action.asserting(_ shouldBe Map(createNewStage -> 0))
    }

    "use bag 5 times" in {
      val ammunition = Ammunition(0, 0, 0, 0, 0)
      val action = for {
        pgGame <- pgGame
        ammunitionNew = pgGame.giveBagEquipment(5, ammunition)
      } yield ammunitionNew
      action.asserting(_ shouldBe Ammunition(1, 1, 1, 1, 1))
    }

    "update stage with Bag" in {
      val element: Symbol = Bag
      val action = for {
        pgGame <- pgGame
        stage <- pgGame.updateStage(element, customStage)
      } yield stage
      action.asserting(_ shouldBe Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(1, 2, 0, 0, 0)), 1))
    }
  }
}
