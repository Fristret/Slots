package server

import Game.RPGElements.Stage
import Game.SlotObjects.Configure

import java.time.Instant

object Protocol {

  case class Login(value: String) extends AnyVal
  case class Password(value: String) extends AnyVal
  case class Mail(value: String) extends AnyVal

  trait MessageIn
  case class Player(login: Login, password: Password) extends MessageIn
  case class NewPlayer(mail: Mail, player: Player) extends MessageIn
  case class Bet(amount: Int) extends MessageIn
  case class Balance(message: String) extends MessageIn

  trait MessageOut
  case class BalanceDemo(amountBalance: BigDecimal) extends MessageOut
  case class Combination(value: List[Int]) extends MessageOut
  case class Win(value: Int, list: Configure, stage: Stage) extends MessageOut
  case class WinOutput(login: String, amountWin: Int, time: Instant) extends MessageOut
}
