package server

import java.time.Instant

object Protocol {

  trait Value
  case class Login(login: String) extends Value
  case class Password(password: String) extends Value
  case class Mail(mail: String) extends Value

  trait MessageIn
  case class Player(login: Login, password: Password) extends MessageIn
  case class NewPlayer(mail: Mail, player: Player) extends MessageIn
  case class Bet(amount: Int) extends MessageIn
  case class Balance(message: String) extends MessageIn

  trait MessageOut
  case class BalanceDemo(amountBalance: BigDecimal) extends MessageOut
  case class Combination(comb: List[Int]) extends MessageOut
  case class Win(win: Int) extends MessageOut
  case class WinOutput(login: String, amountWin: Win, time: Instant) extends MessageOut
}
