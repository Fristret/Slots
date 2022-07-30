package server

import Protocol._

object CheckSyntax {

  trait Syntax[A] {
    def check(value: A): Either[String, Unit]
  }

  implicit val mailSyntax: Syntax[Mail] = (value: Mail) => if (value.mail.matches("[a-zA-Z0-9]+@[a-z]+[.][a-z]+")) Right()
  else Left("Wrong mail")

  implicit val loginSyntax: Syntax[Login] = (value: Login) => if (value.login.length >= 3 && value.login.length <= 15 && value.login.matches("[a-zA-Z0-9]+")) Right()
  else Left("Wrong login")

  implicit val passwordSyntax: Syntax[Password] = (value: Password) => if (value.password.length >= 6 && value.password.length <= 15 && value.password.matches("[a-zA-Z0-9]+")) Right()
  else Left("Wrong password")

  implicit val playerSyntax: Syntax[Player] = (value: Player) => value.login.checkSyntax match {
    case Right(_) => value.password.checkSyntax
    case Left(err) => Left(err)
  }

  implicit val newPlayerSyntax: Syntax[NewPlayer] = (value: NewPlayer) => value.mail.checkSyntax match {
    case Right(_) => value.player.checkSyntax
    case Left(err) => Left(err)
  }
  implicit val betSyntax: Syntax[Bet] = (bet: Bet) => if (bet.amount >= 40 && bet.amount <= 10000) Right()
  else Left("Wrong bet")

  implicit class SyntaxOps[A](value: A) {
    def checkSyntax(implicit syntax: Syntax[A]): Either[String, Unit] = {
      syntax.check(value)
    }
  }
}
