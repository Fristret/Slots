package game

import cats.Monad
import cats.effect._
import cats.implicits._
import fs2.concurrent.Topic
import game.models.SlotObjects._
import game.utils.RNG
import game.models.MiniGameObjects._
import io.circe.syntax.EncoderOps
import server.json.MessageJson.miniGameOutputEncoder


trait SlotMiniGame[F[_]] {
  def play: F[List[Symbol]]
}

object SlotMiniGame {

  def apply[F[_] : Monad : Sync](topicClient: Topic[F, String], hero: MiniGameUnit): SlotMiniGame[F] = new SlotMiniGame[F] {

    def generateUnitEnemy: F[MiniGameUnit] = {
      val rng = RNG.apply[F]
      rng.generatorMiniGame.map {
        case 1 => Leaf
        case 2 => Water
        case 3 => Air
        case 4 => Fire
        case _ => ZaWarudo
      }
    }

    def play: F[List[Symbol]] = for {
      enemy <- generateUnitEnemy
      res = hero match {
        case Leaf | ZaWarudo => enemy match {
          case Water => List(Action)
          case _ => List(NoSymbol)
        }
        case Water => enemy match {
          case Fire => List(Bag)
          case _ => List(NoSymbol)
        }
        case Fire => enemy match {
          case Leaf => List(Sword, Sword, Sword)
          case _ => List(NoSymbol)
        }
        case Air => List(Chest)
        case _ => List(NoSymbol)
      }
      _ <- topicClient.publish1(MiniGameOutput(hero, enemy, res).asJson.noSpaces)
    } yield res

  }
}
