package game

import game.models.RPGElements._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import game.models.SlotObjects._
import server.models.Protocol._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.catsSyntaxSemigroup
import fs2.concurrent.Topic
import game.RPG.createNewStage
import game.models.MiniGameObjects.Leaf
import ForSlotTests._


class SlotTests extends AsyncFreeSpec with Matchers with AsyncIOSpec{

  val emptyRef: IO[Ref[IO, Map[Login, Stage]]] = Ref[IO].of(Map.empty[Login, Stage])
  val notEmptyRef: IO[Ref[IO, Map[Login, Stage]]] = Ref[IO].of(Map(playerLogin -> createNewStage, playerLogin2 -> customStage))
  val topic: IO[Topic[IO, String]] = Topic[IO, String]("")
  val slot: IO[Slot[IO]] = for {
    ref <- notEmptyRef
    topic <- topic
  } yield Slot(Bet(20), playerLogin, topic, ref, Leaf)

  "Slot should" - {
    "win = 0 and win = 6" in{
      val action = for{
        slot <- slot
        map = slot.getPayMap(slot.formPayMap(noElementScreen))
        map2 = slot.getPayMap(slot.formPayMap(point5RowScreen))
      }yield (map, map2)
      action.asserting(_._1 shouldBe Map.empty[List[Symbol], Int])
      action.asserting(_._2 shouldBe Map(List(Point5, Point5, Point5, NoSymbol, NoSymbol) -> 6))
    }

    "sum win should be less than bet" in {
      val action = for {
        slot <- slot
        win100 <- spinRepeatByTimes(slot.spin, 100)
      } yield win100
      action.asserting(_ should be <= 20 * 100)
    }

    "Random should" - {
      "Jackpot chance less than 3%" in {
        val action = for {
          slot <- slot
          count1 <- spinRepeatByBoolean(slot.spin, Jackpot)
          count2 <- spinRepeatByBoolean(slot.spin, Jackpot)
          count3 <- spinRepeatByBoolean(slot.spin, Jackpot)
          chance = 1000/ ((count1 + count2 + count3)/3)
        } yield chance
        action.asserting(_ should be <= 3)
      }

      "Action chance less than 5%" in {
        val action = for {
          slot <- slot
          count1 <- spinRepeatByBoolean(slot.spin, Action)
          count2 <- spinRepeatByBoolean(slot.spin, Action)
          count3 <- spinRepeatByBoolean(slot.spin, Action)
          chance = 100/ ((count1 + count2 + count3)/3)
        } yield chance
        action.asserting(_ should be <= 5)
      }
    }
  }

  def spinRepeatByBoolean(f: => IO[SlotExit], element: Symbol, count: Int = 1): IO[Int] = for {
    exit <- f
    count <- if (exit.line.contains(element)) IO(count)
      else spinRepeatByBoolean(f, element, count = count + 1)
  } yield count

  def spinRepeatByTimes(f: => IO[SlotExit], times: Int, count: Int = 1): IO[Int] = {for {
    win <- f
  } yield win.value} |+| (if (count >= times) IO(0) else spinRepeatByTimes(f, times, count + 1))

}

object ForSlotTests {
  val noElementScreen: Reel = Reel(List(Column(Map(1 -> NoSymbol, 2 -> NoSymbol, 3 -> NoSymbol)), Column(Map(1 -> Point5, 2 -> Wild, 3 -> Point10)), Column(Map(1 -> Wild, 2 -> FreeSpins, 3 -> Point10)), Column(Map(1 -> Point5, 2 -> Point10, 3 -> Point10)), Column(Map(1 -> Point10, 2 -> Point10, 3 -> Point5))))
  val point5RowScreen: Reel = Reel(List(Column(Map(1 -> Point5, 2 -> NoSymbol, 3 -> NoSymbol)), Column(Map(1 -> Point5, 2 -> NoSymbol, 3 -> NoSymbol)), Column(Map(1 -> Point5, 2 -> NoSymbol, 3 -> NoSymbol)), Column(Map(1 -> NoSymbol, 2 -> NoSymbol, 3 -> NoSymbol)), Column(Map(1 -> NoSymbol, 2 -> NoSymbol, 3 -> NoSymbol))))
  val customScreen: Reel = Reel(List(Column(Map(1 -> Action, 2 -> Sword, 3 -> Point5)), Column(Map(1 -> Point5, 2 -> Wild, 3 -> Point10)), Column(Map(1 -> Wild, 2 -> FreeSpins, 3 -> Point10)), Column(Map(1 -> Point5, 2 -> Point10, 3 -> Point10)), Column(Map(1 -> Point10, 2 -> Point10, 3 -> Point5))))
  val playerLogin: Login = Login("masana")
  val playerLogin2: Login = Login("masana234")
  val customStage: Stage = Stage(2, 3, Enemy(Mob, 2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1)
}
