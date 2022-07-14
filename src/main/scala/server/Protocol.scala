package server

import java.time.Instant

object Protocol {
  case class Login(login: String)

  trait MessageIn
  case class Player(login: String, password: String) extends MessageIn
  case class Bet(amountBet: Int, multiple: Int) extends MessageIn

  trait MessageOut
  case class Balance(amountBalance: BigDecimal) extends MessageOut
  case class Combination(comb: List[Int]) extends MessageOut
  case class Win(win: BigDecimal) extends MessageOut
  case class WinOutput(login: Login, amountWin: Win, time: Instant) extends MessageOut
}
