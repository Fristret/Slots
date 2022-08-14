package game

import cats._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import fs2.concurrent.Topic
import game.RPG.createNewStage
import game.models.MiniGameObjects.MiniGameUnit
import game.models.RPGElements.Stage
import game.models.SlotObjects._
import game.utils.CheckScreen._
import game.utils.RNG
import game.utils.SaveMethods._
import io.circe.syntax.EncoderOps
import server.json.MessageJson._
import server.models.Protocol._

trait Slot[F[_]]{
  def formPayMap(screen: Reel): Map[Int, Map[List[Symbol], Int]]
  def getPayMap(map: Map[Int, Map[List[Symbol], Int]]): Map[List[Symbol], Int]
  def paymentCheck(list: List[Symbol]): Map[List[Symbol], Int]
  def getElements(list: List[List[Symbol]]): List[Symbol]
  def checkWin(screen: Reel): F[SlotExit]
  def spin: F[SlotExit]
}

object Slot {

  def apply[F[_]: Monad : Sync](bet: Bet, login: Login, topicClient: Topic[F, String], rpgProgress: Ref[F, Map[Login, Stage]], miniGameUnit: MiniGameUnit): Slot[F] = new Slot[F] {
    def formPayMap(screen: Reel): Map[Int, Map[List[Symbol], Int]] = {
        Map(1 -> paymentCheck(checkRow(1, 1, screen.column))) |+|
        Map(2 -> paymentCheck(checkRow(2, 1, screen.column))) |+|
        Map(3 -> paymentCheck(checkRow(3, 1, screen.column))) |+|
        Map(4 -> paymentCheck(check2(1, 1, screen.column))) |+|
        Map(5 -> paymentCheck(check2(3, 1, screen.column))) |+|
        Map(6 -> paymentCheck(check3(1, 1, screen.column))) |+|
        Map(7 -> paymentCheck(check3(3, 1, screen.column))) |+|
        Map(8 -> paymentCheck(check4(1, 1, screen.column))) |+|
        Map(9 -> paymentCheck(check4(3, 1, screen.column))) |+|
        Map(10 -> paymentCheck(check5(1, screen.column))) |+|
        Map(11 -> paymentCheck(check6(1, 1, screen.column))) |+|
        Map(12 -> paymentCheck(check6(3, 1, screen.column))) |+|
        Map(13 -> paymentCheck(check7(2, 1, screen.column))) |+|
        Map(14 -> paymentCheck(check7(3, 1, screen.column))) |+|
        Map(15 -> paymentCheck(check8(1, screen.column)))
    }

    def getPayMap(map: Map[Int, Map[List[Symbol], Int]]): Map[List[Symbol], Int] = {
       map.values.toList.filter(map => map.nonEmpty).foldLeft(Map.empty[List[Symbol], Int])((a, b) => a |+| b)
    }

    def paymentCheck(list: List[Symbol]): Map[List[Symbol], Int] = {
      val map = Map.empty[List[Symbol], Int]
      if (list.isEmpty) map
      else {
        val newList: List[Symbol] = list.indexOf(Wild) match {
          case 0 => list.map {
            case Wild => Option(list(1)) match {
              case Some(x) => x
              case None => Wild
            }
            case x => x
          }
          case -1 => list
          case _ => list.map {
            case Wild => list.headOption match {
              case Some(x) => x
              case None => Wild
            }
            case x => x
          }
        }

        newList match {
          case List(Point5, Point5, Point5, Point5, Point5) => map.updated(list, (bet.amount * 0.4).toInt)
          case List(Point5, Point5, Point5, Point5, _) => map.updated(list, (bet.amount * 0.35).toInt)
          case List(Point5, Point5, Point5, _, _) => map.updated(list, (bet.amount * 0.3).toInt)
          case List(Point10, Point10, Point10, Point10, Point10) => map.updated(list, (bet.amount * 0.7).toInt)
          case List(Point10, Point10, Point10, Point10, _) => map.updated(list, (bet.amount * 0.65).toInt)
          case List(Point10, Point10, Point10, _, _) => map.updated(list, (bet.amount * 0.6).toInt)
          case List(Sword, Sword, Sword, Sword, Sword) => map.updated(list, (bet.amount * 0.6).toInt)
          case List(Sword, Sword, Sword, Sword, _) => map.updated(list, (bet.amount * 0.4).toInt)
          case List(Sword, Sword, Sword, _, _) => map.updated(list, (bet.amount * 0.3).toInt)
          case List(Bag, Bag, Bag, Bag, Bag) => map.updated(list, (bet.amount * 0.8).toInt)
          case List(Bag, Bag, Bag, Bag, _) => map.updated(list, (bet.amount * 0.5).toInt)
          case List(Bag, Bag, Bag, _, _) => map.updated(list, (bet.amount * 0.2).toInt)
          case List(Chest, Chest, Chest, Chest, Chest) => map.updated(list, (bet.amount * 0.8).toInt)
          case List(Chest, Chest, Chest, Chest, _) => map.updated(list, (bet.amount * 0.5).toInt)
          case List(Chest, Chest, Chest, _, _) => map.updated(list, (bet.amount * 0.2).toInt)
          case List(Jackpot, Jackpot, Jackpot, Jackpot, Jackpot) => map.updated(list, bet.amount * 500)
          case List(Jackpot, Jackpot, Jackpot, Jackpot, _) => map.updated(list, bet.amount * 200)
          case List(Jackpot, Jackpot, Jackpot, _, _) => map.updated(list, bet.amount * 100)
          case List(FreeSpins, FreeSpins, FreeSpins, FreeSpins, FreeSpins) => map.updated(list, (bet.amount * 0.8).toInt)
          case List(FreeSpins, FreeSpins, FreeSpins, FreeSpins, _) => map.updated(list, (bet.amount * 0.5).toInt)
          case List(FreeSpins, FreeSpins, FreeSpins, _, _) => map.updated(list, (bet.amount * 0.2).toInt)
          case List(MiniGame, MiniGame, MiniGame, MiniGame, MiniGame) => map.updated(list, (bet.amount * 0.8).toInt)
          case List(MiniGame, MiniGame, MiniGame, MiniGame, _) => map.updated(list, (bet.amount * 0.5).toInt)
          case List(MiniGame, MiniGame, MiniGame, _, _) => map.updated(list, (bet.amount * 0.2).toInt)
          case List(Action, Action, Action, Action, Action) => map.updated(list, (bet.amount * 0.8).toInt)
          case List(Action, Action, Action, Action, _) => map.updated(list, (bet.amount * 0.5).toInt)
          case List(Action, Action, Action, _, _) => map.updated(list, (bet.amount * 0.2).toInt)
          case _ => map
        }
      }
    }

    def getElements(list: List[List[Symbol]]): List[Symbol] = list.headOption match {
      case None => List()
      case Some(x) => List(x.headOption match {
        case Some(value) => value
        case None => NoSymbol
      }) ++ getElements(list.tailSave)
    }


    def filterFormedPayMap(map: Map[Int, Map[List[Symbol], Int]]): Map[Int, Map[List[Symbol], Int]] = {
      map.filter(a => a._2.nonEmpty)
    }

    def checkWin(screen: Reel): F[SlotExit] = {
      val payMapFormed = formPayMap(screen)
      val payLine = PayLine(filterFormedPayMap(payMapFormed).keys.toList.sorted)
      val payMap = getPayMap(payMapFormed)
      val payment = payMap.foldLeft(0)(_ + _._2)
      val winningCombinations = payMap.keys.toList
      val listRPGAction = getElements(winningCombinations)
      val rpg = RPG(listRPGAction, login, bet, topicClient, rpgProgress, miniGameUnit)
      for {
        _ <- topicClient.publish1(payLine.asJson.noSpaces)
        _ <- topicClient.publish1(Win(payment).asJson.noSpaces)
        rewardsRPG <- rpg.playRPG
        stageRPG = rewardsRPG.keys.toList.headOption match {
          case None => createNewStage
          case Some(x) => x
        }
        _ <- topicClient.publish1(stageRPG.asJson.noSpaces)
        winRPG = rewardsRPG.get(stageRPG) match {
          case None => 0
          case Some(value) => value
        }
        _ <- topicClient.publish1(Win(winRPG).asJson.noSpaces)
      }
      yield SlotExit(payment + winRPG, listRPGAction, listRPGAction.contains(FreeSpins))
    }

    def spin: F[SlotExit] = {
      val generator = RNG.apply[F]
      for {
        screen <- generator.generateScreen
        _ <- topicClient.publish1(screen.asJson.noSpaces)
        exit <- checkWin(screen)
      } yield exit
    }

  }


}
