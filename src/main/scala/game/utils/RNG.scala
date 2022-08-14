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
  def getElement: F[Symbol]
  def generateColumn: F[Column]
  def generateScreen: F[Reel]
}

object RNG {

  def apply[F[_] : Monad : Sync]: RNG[F] = new RNG[F] {
    def generator: F[Int] = Sync[F].delay(Random.between(1, 101))

    def generatorRPG: F[Int] = Sync[F].delay(Random.between(1, 11))

    def generatorMiniGame: F[Int] = Sync[F].delay(Random.between(1, 5))

    def getElement: F[Symbol] = for {
      int <- generator
      res = int match {
        case i if i % 10 == 1 && i < 30 => Jackpot //3%
        case i if i % 10 == 1 && i > 50 => Action // 5%
        case i if i % 10 == 2 && i > 50 => FreeSpins //5%
        case i if i % 10 == 3 && i < 100 => Chest //10%
        case i if i % 10 == 6 && i < 100 => Bag //10%
        case i if i % 10 == 4 || i % 10 == 5 && i < 50 => Sword //15%
        case i if i % 10 == 7 && i < 100 => MiniGame //10%
        case i if i % 10 == 8 || i % 10 == 9 && i > 80 => Point10 // 17%
        case i if i % 10 == 9 && i < 50 => Wild //5%
        case _ => Point5 //20 %
      }
    } yield res

    def generateColumn: F[Column] = for {
      element1 <- getElement
      element2 <- getElement
      element3 <- getElement
    }yield Column(Map(1 -> element1) ++ Map(2 -> element2) ++ Map(3 -> element3))

    def generateScreen: F[Reel] = for {
      column1 <- generateColumn
      column2 <- generateColumn
      column3 <- generateColumn
      column4 <- generateColumn
      column5 <- generateColumn
    } yield Reel(List(column1, column2, column3, column4, column5))

  }
}
