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
  val customStage: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1)
  val emptyRef: IO[Ref[IO, Map[Login, Stage]]] = Ref[IO].of(Map.empty[Login, Stage])
  val notEmptyRef: IO[Ref[IO, Map[Login, Stage]]] = Ref[IO].of(Map(playerLogin -> createNewStage, playerLogin2 -> customStage))


  "creating new RPG Game"  - {
    "create" in {
      val map = Map(playerLogin -> createNewStage)
      val action = for {
        ref <- emptyRef
        pgGame = RPG2(List(Action, Sword, Point5, Point5), playerLogin, Bet(200), ref)
        value <- pgGame.createNewRPG
        _ <- IO(println(ref))
      } yield value
      action.unsafeRunSync() should be (map)
    }

    "return exists" in {
      for {
        ref <- notEmptyRef
        pgGame = RPG2(List(Action, Sword, Point5, Point5), playerLogin, Bet(200), ref)
        value <- pgGame.createNewRPG
      } yield value should be (customStage)
    }
  }

//  "enemy attack" - {
//    "success damage" in {
//      val stage: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, -1)), 1)
//      pgGame = RPG2(List(Action, Sword, Point5, Point5), playerLogin, Bet(200), ref)
//      enemyAttack(stage.enemy, stage.hero) should be (Hero(1, 1, Ammunition(0, 2, 0, 0, -1)))
//    }
//
//    "evade damage" in {
//      val stage: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 10)), 1)
//      enemyAttack(stage.enemy, stage.hero) should be (Hero(2, 1, Ammunition(0, 2, 0, 0, 10)))
//    }
//
//    "shield block" in {
//      val stage: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 1, 0)), 1)
//      enemyAttack(stage.enemy, stage.hero) should be (Hero(2, 1, Ammunition(0, 2, 0, 0, 0)))
//    }
//  }
//
//  "hero attack" - {
//    "success damage" in {
//      val stage: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1)
//      heroAttack(stage.enemy, stage.hero) should be (Enemy(Mob, -1, 1, 0))
//    }
//    "success evade" in {
//      val stage: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 10), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1)
//      heroAttack(stage.enemy, stage.hero) should be (Enemy(Mob, 2, 1, 10))
//    }
//  }
//
//  "fight" - {
//    "No dmg" in {
//      val stage: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1)
//      fight(NotDoDmgOrDo = true, stage, Bet(20)) should be (Map(Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 2) -> 0))
//    }
//    "Kill enemies" in {
//      val stage1: Stage = Stage(1, 1, Enemy(Mob, 1, 1, 0), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 1)
//      val stage11: Stage = Stage(5, 1, Enemy(Mob, 1, 1, 0), Hero(2, 1, Ammunition(0, 10, 2, 0, 0)), 1)
//      val stage2: Stage = Stage(6, 3, Enemy(MiniBoss, 1, 1, 0), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 1)
//      val stage3: Stage = Stage(11, 3, Enemy(Boss, 1, 1, 0), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 1)
//      fight(NotDoDmgOrDo = false, stage1, Bet(20)) should be (Map(Stage(2, 1, Enemy(Mob, 3, 1, 1), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 0) -> (0.2 * stage1.level * 20).toInt))
//      fight(NotDoDmgOrDo = false, stage11, Bet(200)) should be (Map(Stage(6, 1, Enemy(MiniBoss, 5, 1, 2), Hero(2, 1, Ammunition(0, 10, 2, 0, 0)), 0) -> (0.2 * 200 * stage11.level * (1 + 0.1 * stage11.hero.ammunition.bag + 1)).toInt))
//      fight(NotDoDmgOrDo = false, stage2, Bet(20)) should be (Map(Stage(7, 3,Enemy(Mob, 5, 1, 1), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 0) -> 0.5 * 20 * pow(2, stage2.level)))
//      fight(NotDoDmgOrDo = false, stage3, Bet(20)) should be (Map(Stage(1, 4, Enemy(Mob, 6, 1, 1), Hero(2, 1, Ammunition(0, 0, 0, 0, 0)), 0) -> 20 * pow(5, stage3.level)))
//    }
//    "Hero Death" in {
//      val stage: Stage = Stage(1, 1, Enemy(Mob, 2, 1, 0), Hero(1, 0, Ammunition(0, 0, 0, 0, -1)), 7)
//      fight(NotDoDmgOrDo = false, stage, Bet(20)) should be (Map(createNewStage -> 0))
//    }
//  }
//
//  "give bag" - {
//    "5 times" in {
//      val ammunition = Ammunition(0, 0, 0, 0, 0)
//      giveBagEquipment(5, ammunition) should be (Ammunition(1, 1, 1, 1, 1))
//    }
//  }
//
//  "update stage" - {
//    "sword" in {
//      val list: List[Element] = List(Sword, Sword, Sword, Point5, Point5)
//      updateStage(list,customStage) should be (Stage(2, 3, Enemy(Mob, -7, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1))
//    }
//  }
}
