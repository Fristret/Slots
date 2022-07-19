package server

import java.time.Instant

object Protocol {
  case class Message(message: String)

  trait MessageIn
  case class Player(message: String, login: String, password: String) extends MessageIn
  case class Bet(amount: Int, multiple: Int) extends MessageIn

  trait MessageOut
  case class Balance(amountBalance: BigDecimal) extends MessageOut
  case class Combination(comb: List[Int]) extends MessageOut
  case class Win(win: BigDecimal) extends MessageOut
  case class WinOutput(login: String, amountWin: Win, time: Instant) extends MessageOut
}
