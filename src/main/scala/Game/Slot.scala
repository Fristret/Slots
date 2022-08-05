package Game

import SlotObjects._
import RNG._
import cats.effect._
import server.Protocol.{Bet, Login, Win}
import CheckScreen._
import Game.RPG.{createNewStage, playRPG}
import Game.RPGElements.Stage

import scala.annotation.tailrec
import SaveMethods._
import cats.effect.concurrent.Ref
import cats.implicits.catsSyntaxSemigroup

object Slot{

  def getObject: Element = generator match {
    case i if i % 10 == 1 && i < 10 => Jackpot //1%
    case i if i % 10 == 1 && i > 40 => Action // 6%
    case i if i % 10 == 7 && i > 40 => FreeSpins //6%
    case i if i % 10 == 3 && i < 100=> Chest //10%
    case i if i % 10 == 6 && i < 100=> Bag //10%
    case i if i % 10 == 4 && i < 100=> Sword //10%
    case i if i % 10 == 7 && i < 40 => MiniGame //4%
    case i if i % 10 == 8 || i % 10 == 7 => Point10// 20%
    case i if i % 10 == 9 && i < 50 => Wild //5%
    case _ => Point5 //28 %
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

  def winingMap(screen: Screen, bet: Bet): Map[List[Element], Int] = {
      paymentCheck(checkRow(1, 1, screen.value), bet) |+|
      paymentCheck(checkRow(2, 1, screen.value), bet) |+|
        paymentCheck(checkRow(3, 1, screen.value), bet) |+|
          paymentCheck(check2(1, 1, screen.value), bet) |+|
            paymentCheck(check2(3, 1, screen.value), bet) |+|
              paymentCheck(check3(1, 1, screen.value), bet) |+|
                paymentCheck(check3(3, 1, screen.value), bet) |+|
                  paymentCheck(check4(1, 1, screen.value), bet) |+|
                    paymentCheck(check4(3, 1, screen.value), bet) |+|
                      paymentCheck(check5(1, screen.value), bet) |+|
                        paymentCheck(check6(1, 1, screen.value), bet) |+|
                          paymentCheck(check6(3, 1, screen.value), bet) |+|
                            paymentCheck(check7(2, 1, screen.value), bet) |+|
                              paymentCheck(check7(3, 1, screen.value), bet) |+|
                                paymentCheck(check8(1, screen.value), bet)
  }

  def paymentCheck(list:List[Element], bet: Bet): Map[List[Element], Int] = {
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
        case _ => list.map{
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
        case List(Point5, Point5, Point5, Point5, _) => map.updated(list,(bet.amount * 0.35).toInt)
        case List(Point5, Point5, Point5, _, _) => map.updated(list,(bet.amount * 0.3).toInt)
        case List(Point10, Point10, Point10, Point10, Point10) => map.updated(list,(bet.amount * 0.8).toInt)
        case List(Point10, Point10, Point10, Point10, _) => map.updated(list,(bet.amount * 0.7).toInt)
        case List(Point10, Point10, Point10, _, _) => map.updated(list,(bet.amount * 0.6).toInt)
        case List(Sword, Sword, Sword, Sword, Sword) => map.updated(list,(bet.amount * 0.8).toInt)
        case List(Sword, Sword, Sword, Sword, _) => map.updated(list,(bet.amount * 0.7).toInt)
        case List(Sword, Sword, Sword, _, _) => map.updated(list,(bet.amount * 0.6).toInt)
        case List(Bag, Bag, Bag, Bag, Bag) => map.updated(list,(bet.amount * 0.8).toInt)
        case List(Bag, Bag, Bag, Bag, _) => map.updated(list,(bet.amount * 0.7).toInt)
        case List(Bag, Bag, Bag, _, _) => map.updated(list,(bet.amount * 0.6).toInt)
        case List(Chest, Chest, Chest, Chest, Chest) => map.updated(list, (bet.amount * 0.8).toInt)
        case List(Chest, Chest, Chest, Chest, _) => map.updated(list,(bet.amount * 0.7).toInt)
        case List(Chest, Chest, Chest, _, _) => map.updated(list,(bet.amount * 0.6).toInt)
        case List(Jackpot, Jackpot, Jackpot, Jackpot, Jackpot) => map.updated(list, bet.amount * 1000)
        case List(Jackpot, Jackpot, Jackpot, Jackpot, _) => map.updated(list, bet.amount * 500)
        case List(Jackpot, Jackpot, Jackpot, _, _) => map.updated(list, bet.amount * 100)
        case List(FreeSpins, FreeSpins, FreeSpins, FreeSpins, FreeSpins) => map.updated(list,(bet.amount * 1.5).toInt)
        case List(FreeSpins, FreeSpins, FreeSpins, FreeSpins, _) => map.updated(list,(bet.amount * 1.2).toInt)
        case List(FreeSpins, FreeSpins, FreeSpins, _, _) => map.updated(list,(bet.amount * 1.1).toInt)
        case List(MiniGame, MiniGame, MiniGame, MiniGame, MiniGame) => map.updated(list,(bet.amount * 0.75).toInt)
        case List(MiniGame, MiniGame, MiniGame, MiniGame, _) => map.updated(list,(bet.amount * 0.7).toInt)
        case List(MiniGame, MiniGame, MiniGame, _, _) => map.updated(list,(bet.amount * 0.6).toInt)
        case List(Action, Action, Action, Action, Action) => map.updated(list,(bet.amount * 1.6).toInt)
        case List(Action, Action, Action, Action, _) => map.updated(list,(bet.amount * 1.2).toInt)
        case List(Action, Action, Action, _, _) => map.updated(list,(bet.amount * 0.8).toInt)
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

  def checkWin(screen: Screen, login: Login, bet: Bet, rpgProgress: Ref[IO, Map[Login, Stage]]): Win = {
    val payment = winingMap(screen, bet)
    val listWithWin = payment.keys.toList
    val listRPGAction = getElements(listWithWin)
    val rewardsRPG = playRPG(listRPGAction, login, bet, rpgProgress).unsafeRunSync()
    val stageRPG = rewardsRPG.keys.toList.headOption match {
      case None => createNewStage
      case Some(x) => x
    }
    val winRPG = rewardsRPG.get(stageRPG) match {
      case None => 0
      case Some(value) => value
    }
    Win(payment.foldLeft(0)(_ +_._2) + winRPG, Configure(listWithWin), stageRPG, listRPGAction.contains(FreeSpins))
  }

  def spin(bet: Bet, login: Login, rpgProgress: Ref[IO, Map[Login, Stage]]): IO[Win] = {
    val screen = generateScreen
    IO(checkWin(screen, login, bet, rpgProgress))
  }
}
