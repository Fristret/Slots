package server

import Protocol._

object CheckSyntax {

  trait Syntax[A] {
    def check(value: A): Either[String, Unit]
  }

  implicit val mailSyntax: Syntax[Mail] = (mail: Mail) => if (mail.value.matches("[a-zA-Z0-9]+@[a-z]+[.][a-z]+")) Right()
  else Left("Wrong mail")

  implicit val loginSyntax: Syntax[Login] = (login: Login) => if (login.value.length >= 3 && login.value.length <= 15 && login.value.matches("[a-zA-Z0-9]+")) Right()
  else Left("Wrong login")

  implicit val passwordSyntax: Syntax[Password] = (password: Password) => if (password.value.length >= 6 && password.value.length <= 15 && password.value.matches("[a-zA-Z0-9]+")) Right()
  else Left("Wrong password")

  implicit val playerSyntax: Syntax[Player] = (player: Player) => player.login.checkSyntax match {
    case Right(_) => player.password.checkSyntax
    case Left(err) => Left(err)
  }

  implicit val newPlayerSyntax: Syntax[NewPlayer] = (newPlayer: NewPlayer) => newPlayer.mail.checkSyntax match {
    case Right(_) => newPlayer.player.checkSyntax
    case Left(err) => Left(err)
  }
  implicit val betSyntax: Syntax[Bet] = (bet: Bet) => if (bet.amount >= 10 && bet.amount <= 10000) Right()
  else Left("Wrong bet")

  implicit class SyntaxOps[A](value: A) {
    def checkSyntax(implicit syntax: Syntax[A]): Either[String, Unit] = {
      syntax.check(value)
    }
  }
}
