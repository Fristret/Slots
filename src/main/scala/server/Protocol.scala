package server

import Game.SlotObjects.Configure

import java.time.Instant

object Protocol {

  case class Login(login: String) extends AnyVal
  case class Password(password: String) extends AnyVal
  case class Mail(mail: String) extends AnyVal

  trait MessageIn
  case class Player(login: Login, password: Password) extends MessageIn
  case class NewPlayer(mail: Mail, player: Player) extends MessageIn
  case class Bet(amount: Int) extends MessageIn
  case class Balance(message: String) extends MessageIn

  trait MessageOut
  case class BalanceDemo(amountBalance: BigDecimal) extends MessageOut
  case class Combination(comb: List[Int]) extends MessageOut
  case class Win(win: Int, list: Configure) extends MessageOut
  case class WinOutput(login: String, amountWin: Win, time: Instant) extends MessageOut
}
