package game

import cats._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import fs2.concurrent.Topic
import game.PRG.createNewStage
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
  def formWiningMap(screen: Screen): Map[Int, Map[List[Element], Int]]
  def getWiningMap(map: Map[Int, Map[List[Element], Int]]): Map[List[Element], Int]
  def paymentCheck(list: List[Element]): Map[List[Element], Int]
  def getElements(list: List[List[Element]]): List[Element]
  def checkWin(screen: Screen): F[SlotExit]
  def spin: F[SlotExit]
}

object Slot {

  def apply[F[_]: Monad : Sync](bet: Bet, login: Login, topicClient: Topic[F, String], rpgProgress: Ref[F, Map[Login, Stage]], miniGameUnit: MiniGameUnit): Slot[F] = new Slot[F] {
    def formWiningMap(screen: Screen): Map[Int, Map[List[Element], Int]] = {
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

    def getWiningMap(map: Map[Int, Map[List[Element], Int]]): Map[List[Element], Int] = {
       map.values.toList.foldLeft(Map.empty[List[Element], Int])((a, b) => a |+| b)
    }

    def paymentCheck(list: List[Element]): Map[List[Element], Int] = {
      val map = Map.empty[List[Element], Int]
      if (list.isEmpty) map
      else {
        val newList: List[Element] = list.indexOf(Wild) match {
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
        case Some(value) => value
        case None => NoElement
      }) ++ getElements(list.tailSave)
    }

    def rec(map: Map[Int, Map[List[Element], Int]], list: List[Map[List[Element], Int]]): Map[Int, Map[List[Element], Int]] = if (list.isEmpty) Map.empty[Int, Map[List[Element], Int]]
    else map.filter(_._2 == list.headOption.getOrElse(List(Map.empty[List[Element], Int])))

    def filterWiningMap(map: Map[Int, Map[List[Element], Int]]): Map[Int, Map[List[Element], Int]] = {
      val revMap = map.flatMap(a => a._2.map{case (a,b) => if (b == 0) Map.empty[List[Element], Int] else Map(a -> b)}).toList
      val listInt = revMap.filterNot(a => a.isEmpty)
      rec(map, listInt)
    }

    def checkWin(screen: Screen): F[SlotExit] = {
      val winingMapFormed = formWiningMap(screen)
      val winingLine = WiningLine(filterWiningMap(winingMapFormed).keys.toList)
      val winingMap = getWiningMap(winingMapFormed)
      val payment = winingMap.foldLeft(0)(_ + _._2)
      val listWithWin = winingMap.keys.toList
      val listRPGAction = getElements(listWithWin)
      val rpg = PRG(listRPGAction, login, bet, topicClient, rpgProgress, miniGameUnit)
      for {
        _ <- topicClient.publish1(winingLine.asJson.noSpaces)
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
      yield SlotExit(payment + winRPG, listRPGAction.contains(FreeSpins))
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
