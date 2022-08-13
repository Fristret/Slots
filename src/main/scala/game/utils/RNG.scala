package game.utils

import cats.effect.Sync
import cats.Monad
import cats.implicits._
import game.models.SlotObjects._

import scala.util.Random

trait RNG[F[_]] {
  def generator: F[Int]
  def generatorRPG: F[Int]
  def generatorMiniGame: F[Int]
  def getElement: F[Element]
  def generateColumn: F[Column]
  def generateScreen: F[Screen]
}

object RNG {

  def apply[F[_] : Monad : Sync]: RNG[F] = new RNG[F] {
    def generator: F[Int] = Sync[F].delay(Random.between(1, 101))

    def generatorRPG: F[Int] = Sync[F].delay(Random.between(1, 11))

    def generatorMiniGame: F[Int] = Sync[F].delay(Random.between(1, 5))

    def getElement: F[Element] = for {
      int <- generator
      res = int match {
        case i if i % 10 == 1 && i < 10 => Jackpot //1%
        case i if i % 10 == 1 && i > 40 => Action // 6%
        case i if i % 10 == 7 && i > 40 => FreeSpins //6%
        case i if i % 10 == 3 && i < 100 => Chest //10%
        case i if i % 10 == 6 && i < 100 => Bag //10%
        case i if i % 10 == 4 && i < 100 => Sword //10%
        case i if i % 10 == 7 && i < 40 => MiniGame //4%
        case i if i % 10 == 8 || i % 10 == 7 => Point10 // 20%
        case i if i % 10 == 9 && i < 50 => Wild //5%
        case _ => Point5 //28 %
      }
    } yield res

    def generateColumn: F[Column] = for {
      element1 <- getElement
      element2 <- getElement
      element3 <- getElement
    }yield Column(Map(1 -> element1) ++ Map(2 -> element2) ++ Map(3 -> element3))

    def generateScreen: F[Screen] = for {
      column1 <- generateColumn
      column2 <- generateColumn
      column3 <- generateColumn
      column4 <- generateColumn
      column5 <- generateColumn
    } yield Screen(List(column1, column2, column3, column4, column5))
  }
}
