package server.models

import cats.effect.IO
import cats.effect.concurrent.Ref
import fs2.concurrent.Topic
import game.models.MiniGameObjects.MiniGameUnit
import game.models.RPGElements.Stage
import server.models.CommonClasses.Token

import java.time.Instant

object Protocol {

  type RPGProgress = Ref[IO, Map[Login, Stage]]
  type Cache = Ref[IO, Map[Token, Instant]]
  type TopicGlobal = Topic[IO, String]
  type TopicClient = Topic[IO, String]
  type MiniGameRef = Ref[IO, MiniGameUnit]

  final case class Login(value: String) extends AnyVal
  final case class Password(value: String) extends AnyVal
  final case class Mail(value: String) extends AnyVal

  sealed trait MessageIn
  final case class Player(login: Login, password: Password) extends MessageIn
  final case class NewPlayer(mail: Mail, player: Player) extends MessageIn
  final case class Bet(amount: Int) extends MessageIn
  final case class MiniGameOption(spirit: MiniGameUnit) extends MessageIn

  sealed trait MessageOut
  final case class Win(payment: Int) extends MessageOut
  final case class WinOutput(login: String, amountWin: Int, time: Instant) extends MessageOut
  final case class BalanceOutput(balance: Int) extends MessageOut
  final case class Message(text: String) extends MessageOut

}
