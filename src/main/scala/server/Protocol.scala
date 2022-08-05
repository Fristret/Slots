package server

import Game.RPGElements.Stage
import Game.SlotObjects.Configure

import java.time.Instant

object Protocol {

  final case class Login(value: String) extends AnyVal
  final case class Password(value: String) extends AnyVal
  final case class Mail(value: String) extends AnyVal

  sealed trait MessageIn
  final case class Player(login: Login, password: Password) extends MessageIn
  final case class NewPlayer(mail: Mail, player: Player) extends MessageIn
  final case class Bet(amount: Int) extends MessageIn
  final case class Balance(message: String) extends MessageIn

  sealed trait MessageOut
  final case class Win(value: Int, list: Configure, stage: Stage, freeSpins: Boolean) extends MessageOut
  final case class WinOutput(login: String, amountWin: Int, time: Instant) extends MessageOut
  final case class BalanceOut(balance: Int) extends MessageOut
}
