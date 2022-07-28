package Game

import SlotObjects._
import RNG._
import cats.effect.{ExitCode, IO, IOApp}
import server.Protocol._

object Slot extends IOApp{

  def getObject: Objects = generator match {
    case i if (i == 1) => Jackpot //1%
    case i if (i % 10 == 2 && i < 50) => FreeSpins //5%
    case i if (i % 10 == 2 && i > 50) => Action // 4%
    case i if (i % 10 == 3 && i < 50) => Chest //5%
    case i if (i % 10 == 6 && i > 50) => Bag //5%
    case i if (i % 5 == 0 && i <= 50) => Sword //15%
    case i if (i % 10 == 6 && i < 50) => MiniGame //5%
    case i if (i % 10 == 7) => Multiply //10%
    case i if (i % 10 == 8 || i % 10 == 4) => Point10 // 20%
    case _ => Point5//30%
  }

  def generateColumn: Column = {
    val object1 = getObject
    val object2 = getObject
    val object3 = getObject
    Column(List(object1, object2, object3))
  }

  def generateScreen: Screen = {
    val column1 = generateColumn
    val column2 = generateColumn
    val column3 = generateColumn
    val column4 = generateColumn
    val column5 = generateColumn
    Screen(List(column1, column2, column3, column4, column5))
  }

  def checkContains(objects: Objects, list: List[Column]): Map[Objects, Int] = ???

  def checkWin(screen: Screen, config: Configure = Configure(Map.empty[Objects, Int])): Configure = screen.screen.head.elements.foldLeft(config)((a, obj) => a(checkContains(obj, screen.screen.tail)))

  //  Column first element ->
  //  Check in next Column, if it contains =>
  //  if Yes => Add to Configure
  //  if No => Take next element from column



  def spin: IO[Screen] = IO(generateScreen)

  override def run(args: List[String]): IO[ExitCode] = for {
    screen <- spin
    _ <- IO(println(screen))
  } yield ExitCode.Success
}
