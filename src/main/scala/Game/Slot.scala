package Game

import SlotObjects._
import RNG._
import cats.effect._
import server.Protocol.{Bet, Login, Win}
import CheckScreen._
import Game.RPG.{createNewRPG, playRPG}
import Game.RPGElements.Stage

import scala.annotation.tailrec
import SaveMethods._
import cats.effect.concurrent.Ref

object Slot{

  def getObject: Element = generator match {
    case i if i == 1 => Jackpot //1%
    case i if i % 10 == 2 && i < 50 => FreeSpins //5%
    case i if i % 10 == 2 && i > 50 => Action // 4%
    case i if i % 10 == 3 && i < 50 => Chest //5%
    case i if i % 10 == 6 && i > 50 => Bag //5%
    case i if i % 5 == 0 && i <= 50 => Sword //15%
    case i if i % 10 == 6 && i < 50 => MiniGame //5%
    case i if i % 10 == 8 || i % 10 == 4 => Point10 // 20%
    case i if i % 10 == 9 => Wild //10%
    case _ => Point5 //30 %
  }

  def generateColumn: Column = {
    val object1 = getObject
    val object2 = getObject
    val object3 = getObject
    Column(Map(1 -> object1) ++ Map(2 -> object2) ++ Map(3 -> object3))
  }

  def generateScreen: Screen = {
    val column1 = generateColumn
    val column2 = generateColumn
    val column3 = generateColumn
    val column4 = generateColumn
    val column5 = generateColumn
    Screen(List(column1, column2, column3, column4, column5))
  }

  def generateConfigure(screen: Screen): Configure = {
    Configure(List(
      checkRow(1, 1, screen.value),
      checkRow(2, 1, screen.value),
      checkRow(3, 1, screen.value),
      check2(1, 1, screen.value),
      check2(3, 1, screen.value),
      check3(1, 1, screen.value),
      check3(3, 1, screen.value),
      check4(1, 1, screen.value),
      check4(3, 1, screen.value),
      check5(1, screen.value),
      check6(1, 1, screen.value),
      check6(3, 1, screen.value),
      check7(2, 1, screen.value),
      check7(3, 1, screen.value),
      check8(1, screen.value)
    )
    )
  }

  @tailrec
  def paymentCheck(list: List[List[Element]], bet: Bet, map: Map[Int, Int], count: Int = 1): Map[Int, Int] = if (list.isEmpty) map
  else {
    val newList: List[Element] = list.headOption match {
      case None => List()
      case Some(value) => value.foldLeft(value)((a, b) => if (a.indexOf(Wild) != 0) b match {
        case Wild => a.updated(a.indexOf(Wild), a.headOption match {
          case None => Point5
          case Some(value) => value
        })
        case _ => a
      }
      else b match {
        case Wild => a.updated(a.indexOf(Wild), a(1))
        case _ => a
      })
    }

    newList match {
      case List(Point5, Point5, Point5, Point5, Point5) => {
        val newMap = map.updated(count, (bet.amount * 0.4).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Point5, Point5, Point5, Point5, _) => {
        val newMap = map.updated(count,(bet.amount * 0.45).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Point5, Point5, Point5, _, _) => {
        val newMap = map.updated(count,(bet.amount * 0.3).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Point10, Point10, Point10, Point10, Point10) => {
        val newMap = map.updated(count,(bet.amount * 0.5).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Point10, Point10, Point10, Point10, _) => {
        val newMap = map.updated(count,(bet.amount * 0.4).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Point10, Point10, Point10, _, _) => {
        val newMap = map.updated(count,(bet.amount * 0.3).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Sword, Sword, Sword, Sword, Sword) => {
        val newMap = map.updated(count,(bet.amount * 0.9).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Sword, Sword, Sword, Sword, _) => {
        val newMap = map.updated(count,(bet.amount * 0.8).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Sword, Sword, Sword, _, _) => {
        val newMap = map.updated(count,(bet.amount * 0.7).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Bag, Bag, Bag, Bag, Bag) => {
        val newMap = map.updated(count,(bet.amount * 0.9).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Bag, Bag, Bag, Bag, _) => {
        val newMap = map.updated(count,(bet.amount * 0.8).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Bag, Bag, Bag, _, _) => {
        val newMap = map.updated(count,(bet.amount * 0.7).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Chest, Chest, Chest, Chest, Chest) => {
        val newMap = map.updated(count,(bet.amount * 2))
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Chest, Chest, Chest, Chest, _) => {
        val newMap = map.updated(count,(bet.amount * 1.6).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Chest, Chest, Chest, _, _) => {
        val newMap = map.updated(count,(bet.amount * 1.5).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Jackpot, Jackpot, Jackpot, Jackpot, Jackpot) => {
        val newMap = map.updated(count,(bet.amount * 1000))
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Jackpot, Jackpot, Jackpot, Jackpot, _) => {
        val newMap = map.updated(count,(bet.amount * 500))
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Jackpot, Jackpot, Jackpot, _, _) => {
        val newMap = map.updated(count,(bet.amount * 100))
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(FreeSpins, FreeSpins, FreeSpins, FreeSpins, FreeSpins) => {
        val newMap = map.updated(count,(bet.amount * 1.5).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(FreeSpins, FreeSpins, FreeSpins, FreeSpins, _) => {
        val newMap = map.updated(count,(bet.amount * 1.2).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(FreeSpins, FreeSpins, FreeSpins, _, _) => {
        val newMap = map.updated(count,(bet.amount * 1.1).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(MiniGame, MiniGame, MiniGame, MiniGame, MiniGame) => {
        val newMap = map.updated(count,(bet.amount * 0.75).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(MiniGame, MiniGame, MiniGame, MiniGame, _) => {
        val newMap = map.updated(count,(bet.amount * 0.7).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(MiniGame, MiniGame, MiniGame, _, _) => {
        val newMap = map.updated(count,(bet.amount * 0.6).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Action, Action, Action, Action, Action) => {
        val newMap = map.updated(count,(bet.amount * 0.3).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Action, Action, Action, Action, _) => {
        val newMap = map.updated(count,(bet.amount * 0.2).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case List(Action, Action, Action, _, _) => {
        val newMap = map.updated(count,(bet.amount * 0.1).toInt)
        paymentCheck(list.tailSave, bet, newMap, count + 1)
      }
      case _ => paymentCheck(list.tailSave, bet,map, count + 1)
    }
  }

  def getWinningConfigure(list: List[List[Element]], set: List[Int]): List[List[Element]] = set.headOption match {
    case Some(x) => List(list(x-1)) ++ getWinningConfigure(list, set.tail)
    case None => List()
  }

  def getElements(list: List[List[Element]]): List[Element] = list.headOption match {
    case None => List()
    case Some(x) => List(x.headOption match {
      case None => NoElement
      case Some(value) => value
    }) ++ getElements(list.tailSave)
  }

  def checkWin(screen: Screen, login: Login, bet: Bet, rpgProgress: Ref[IO, Map[Login, Stage]]): Win = {
    val configure = generateConfigure(screen)
    val payment = paymentCheck(configure.value, bet, Map.empty[Int, Int])
    val listOfKeys = payment.keys.toList
    val listWithWin = getWinningConfigure(configure.value, listOfKeys.tailSave)
    val listRPGAction = getElements(listWithWin)
    val rewardsRPG = playRPG(listRPGAction, login, bet, rpgProgress)
    Win(payment.foldLeft(0)(_ +_._2), Configure(listWithWin), rewardsRPG)
  }

  def spin(bet: Bet, login: Login, rpgProgress: Ref[IO, Map[Login, Stage]]): IO[Win] = {
    val screen = generateScreen
    IO(checkWin(screen, login, bet, rpgProgress))
  }
}
