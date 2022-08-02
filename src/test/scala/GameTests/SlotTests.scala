package GameTests

import Game.RPG._
import Game.RPGElements._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import Game.Slot._
import Game.SlotObjects._
import server.Protocol._
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits.catsSyntaxSemigroup

class SlotTests extends AnyFreeSpec with Matchers{

  val noElementScreen: Screen = Screen(List(Column(Map(1 -> NoElement, 2 -> NoElement, 3 -> NoElement)), Column(Map(1 -> Point5, 2 -> Wild, 3 -> Point10)), Column(Map(1 -> Wild, 2 -> FreeSpins, 3 -> Point10)), Column(Map(1 -> Point5, 2 -> Point10, 3 -> Point10)), Column(Map(1 -> Point10, 2 -> Point10, 3 -> Point5))))
  val point5RowScreen: Screen = Screen(List(Column(Map(1 -> Point5, 2 -> NoElement, 3 -> NoElement)), Column(Map(1 -> Point5, 2 -> NoElement, 3 -> NoElement)), Column(Map(1 -> Point5, 2 -> NoElement, 3 -> NoElement)), Column(Map(1 -> NoElement, 2 -> NoElement, 3 -> NoElement)), Column(Map(1 -> NoElement, 2 -> NoElement, 3 -> NoElement))))
  val customScreen: Screen = Screen(List(Column(Map(1 -> Action, 2 -> Sword, 3 -> Point5)), Column(Map(1 -> Point5, 2 -> Wild, 3 -> Point10)), Column(Map(1 -> Wild, 2 -> FreeSpins, 3 -> Point10)), Column(Map(1 -> Point5, 2 -> Point10, 3 -> Point10)), Column(Map(1 -> Point10, 2 -> Point10, 3 -> Point5))))
  val playerLogin: Login = Login("masana")
  val playerLogin2: Login = Login("masana234")
  val customStage: Stage = Stage(2, 3, Mob(2, 1, 0), Hero(2, 1, Ammunition(0, 2, 0, 0, 0)), 1)
  val emptyRef: IO[Ref[IO, Map[Login, Stage]]] = Ref[IO].of(Map.empty[Login, Stage])
  val notEmptyRef: IO[Ref[IO, Map[Login, Stage]]] = Ref[IO].of(Map(playerLogin -> createNewStage, playerLogin2 -> customStage))


  "winning map" - {
    "0 win, empty list" in{
      winingMap(noElementScreen, Bet(200)) should be (Map.empty[List[Element], Int])
    }
    "win, list(point5, point5, point5, _, _)" in {
      winingMap(point5RowScreen, Bet(20)) should be (Map(List(Point5, Point5, Point5, NoElement, NoElement) -> 6))
    }
  }

  def spinRepeat(f: => IO[Win], times: Int, count: Int = 1): IO[Int] = {for {
    win <- f
    _ <- IO(println(win))
  } yield win.value} |+| (if (count >= times) IO(0) else spinRepeat(f, times, count + 1))

  "play slot" - {
    "100 win" in {
        val ref = notEmptyRef.unsafeRunSync()
        val win100 = spinRepeat(spin(Bet(10), playerLogin, ref), 100).unsafeRunSync()
        println(win100)
        win100 should be <= 10 * 100
      }
  }
}
