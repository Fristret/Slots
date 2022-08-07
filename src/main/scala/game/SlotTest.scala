package game

import cats._
import cats.effect.concurrent.Ref
import cats.implicits._
import game.RPG2.createNewStage
import game.models.RPGElements.Stage
import game.models.SlotObjects._
import game.utils.CheckScreen._
import game.utils.RNG
import game.utils.SaveMethods._
import server.models.Protocol.{Bet, Login, Win}

trait SlotTest[F[_]]{
  def winingMap(screen: Screen): Map[List[Element], Int]
  def paymentCheck(list: List[Element]): Map[List[Element], Int]
  def getElements(list: List[List[Element]]): List[Element]
  def checkWin(screen: Screen): F[Win]
  def spin: F[Win]
}


object SlotTest {

  def apply[F[_]: Applicative : Monad](bet: Bet, login: Login, rpgProgress: Ref[F, Map[Login, Stage]]): SlotTest[F] = new SlotTest[F] {
    def winingMap(screen: Screen): Map[List[Element], Int] = {
        paymentCheck(checkRow(1, 1, screen.value)) |+|
        paymentCheck(checkRow(2, 1, screen.value)) |+|
        paymentCheck(checkRow(3, 1, screen.value)) |+|
        paymentCheck(check2(1, 1, screen.value)) |+|
        paymentCheck(check2(3, 1, screen.value)) |+|
        paymentCheck(check3(1, 1, screen.value)) |+|
        paymentCheck(check3(3, 1, screen.value)) |+|
        paymentCheck(check4(1, 1, screen.value)) |+|
        paymentCheck(check4(3, 1, screen.value)) |+|
        paymentCheck(check5(1, screen.value)) |+|
        paymentCheck(check6(1, 1, screen.value)) |+|
        paymentCheck(check6(3, 1, screen.value)) |+|
        paymentCheck(check7(2, 1, screen.value)) |+|
        paymentCheck(check7(3, 1, screen.value)) |+|
        paymentCheck(check8(1, screen.value))
    }

    def paymentCheck(list: List[Element]): Map[List[Element], Int] = {
      val map = Map.empty[List[Element], Int]
      if (list.isEmpty) map
      else {
        val newList: List[Element] = list.indexOf(Wild) match {
          case 0 => list.map {
            case Wild => Option(list(1)) match {
              case None => Wild
              case Some(x) => x
            }
            case anotherElement => anotherElement
            case _ => NoElement
          }
          case -1 => list
          case _ => list.map {
            case Wild => list.headOption match {
              case None => Wild
              case Some(x) => x
            }
            case anotherElement => anotherElement
            case _ => NoElement
          }
        }

        newList match {
          case List(Point5, Point5, Point5, Point5, Point5) => map.updated(list, (bet.amount * 0.4).toInt)
          case List(Point5, Point5, Point5, Point5, _) => map.updated(list, (bet.amount * 0.35).toInt)
          case List(Point5, Point5, Point5, _, _) => map.updated(list, (bet.amount * 0.3).toInt)
          case List(Point10, Point10, Point10, Point10, Point10) => map.updated(list, (bet.amount * 0.8).toInt)
          case List(Point10, Point10, Point10, Point10, _) => map.updated(list, (bet.amount * 0.7).toInt)
          case List(Point10, Point10, Point10, _, _) => map.updated(list, (bet.amount * 0.6).toInt)
          case List(Sword, Sword, Sword, Sword, Sword) => map.updated(list, (bet.amount * 0.8).toInt)
          case List(Sword, Sword, Sword, Sword, _) => map.updated(list, (bet.amount * 0.7).toInt)
          case List(Sword, Sword, Sword, _, _) => map.updated(list, (bet.amount * 0.6).toInt)
          case List(Bag, Bag, Bag, Bag, Bag) => map.updated(list, (bet.amount * 0.8).toInt)
          case List(Bag, Bag, Bag, Bag, _) => map.updated(list, (bet.amount * 0.7).toInt)
          case List(Bag, Bag, Bag, _, _) => map.updated(list, (bet.amount * 0.6).toInt)
          case List(Chest, Chest, Chest, Chest, Chest) => map.updated(list, (bet.amount * 0.8).toInt)
          case List(Chest, Chest, Chest, Chest, _) => map.updated(list, (bet.amount * 0.7).toInt)
          case List(Chest, Chest, Chest, _, _) => map.updated(list, (bet.amount * 0.6).toInt)
          case List(Jackpot, Jackpot, Jackpot, Jackpot, Jackpot) => map.updated(list, bet.amount * 1000)
          case List(Jackpot, Jackpot, Jackpot, Jackpot, _) => map.updated(list, bet.amount * 500)
          case List(Jackpot, Jackpot, Jackpot, _, _) => map.updated(list, bet.amount * 100)
          case List(FreeSpins, FreeSpins, FreeSpins, FreeSpins, FreeSpins) => map.updated(list, (bet.amount * 1.5).toInt)
          case List(FreeSpins, FreeSpins, FreeSpins, FreeSpins, _) => map.updated(list, (bet.amount * 1.2).toInt)
          case List(FreeSpins, FreeSpins, FreeSpins, _, _) => map.updated(list, (bet.amount * 1.1).toInt)
          case List(MiniGame, MiniGame, MiniGame, MiniGame, MiniGame) => map.updated(list, (bet.amount * 0.75).toInt)
          case List(MiniGame, MiniGame, MiniGame, MiniGame, _) => map.updated(list, (bet.amount * 0.7).toInt)
          case List(MiniGame, MiniGame, MiniGame, _, _) => map.updated(list, (bet.amount * 0.6).toInt)
          case List(Action, Action, Action, Action, Action) => map.updated(list, (bet.amount * 1.6).toInt)
          case List(Action, Action, Action, Action, _) => map.updated(list, (bet.amount * 1.2).toInt)
          case List(Action, Action, Action, _, _) => map.updated(list, (bet.amount * 0.8).toInt)
          case _ => map
        }
      }
    }

    def getElements(list: List[List[Element]]): List[Element] = list.headOption match {
      case None => List()
      case Some(x) => List(x.headOption match {
        case None => NoElement
        case Some(value) => value
      }) ++ getElements(list.tailSave)
    }

    def checkWin(screen: Screen): F[Win] = {
      val payment = winingMap(screen)
      val listWithWin = payment.keys.toList
      val listRPGAction = getElements(listWithWin)
      val rpg = RPG2(listRPGAction, login, bet, rpgProgress)
      for {
        rewardsRPG <- rpg.playRPG
        stageRPG = rewardsRPG.keys.toList.headOption match {
          case None => createNewStage
          case Some(x) => x
        }
        winRPG = rewardsRPG.get(stageRPG) match {
          case None => 0
          case Some(value) => value
        }
      }
      yield Win(payment.foldLeft(0)(_ + _._2) + winRPG, Configure(listWithWin), stageRPG, screen, listRPGAction.contains(FreeSpins))
    }

    def spin: F[Win] = for {
      _ <- "SR".pure[F]
      generator = RNG.apply[F]
      screen <- generator.generateScreen
      win <- checkWin(screen)
    } yield win

  }


}
