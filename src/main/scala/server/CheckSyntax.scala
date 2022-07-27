package server

import cats.effect.IO
import Protocol._

object CheckSyntax {

  trait Syntax[A] {
    def check(value: A): IO[String]
  }

  def checkBet(value: Int, min: Int, max: Int, div: Int): IO[String] = if ((value >= min && value <= max) && (value % div == 0)) IO(value.toString)
  else IO.raiseError(new IllegalStateException("Wrong bet"))

  implicit val mailSyntax: Syntax[Mail] = (value: Mail) => if (value.mail.matches("[a-zA-Z0-9]+@[a-z]+[.][a-z]+")) IO(value.mail)
  else IO.raiseError(new IllegalStateException("Wrong mail"))

  implicit val loginSyntax: Syntax[Login] = (value: Login) => if (value.login.length >= 3 && value.login.length <= 15 && value.login.matches("[a-zA-Z0-9]+")) IO(value.login)
  else IO.raiseError(new IllegalStateException("Wrong login"))

  implicit val passwordSyntax: Syntax[Password] = (value: Password) => if (value.password.length >= 6 && value.password.length <= 15 && value.password.matches("[a-zA-Z0-9]+")) IO(value.password)
  else IO.raiseError(new IllegalStateException("Wrong password"))

  implicit val playerSyntax: Syntax[Player] = (value: Player) => value.login.checkSyntax *> value.password.checkSyntax

  implicit val newPlayerSyntax: Syntax[NewPlayer] = (value: NewPlayer) => value.mail.checkSyntax *> value.player.checkSyntax

  implicit val betSyntax: Syntax[Bet] = (bet: Bet) => if (bet.amount >= 1 && bet.amount <= 20) IO(bet.amount.toString)
  else IO.raiseError(new IllegalStateException("Wrong bet"))

  implicit class SyntaxOps[A](value: A) {
    def checkSyntax(implicit syntax: Syntax[A]): IO[String] = {
      syntax.check(value)
    }
  }
}
