package server.service

import cats.effect.{IO, IOApp}
import doobie.implicits._
import doobie.util.transactor.Transactor
import server.models.Protocol._

object Doobie extends IOApp.Simple {

  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql://localhost:5432/SlotDB",
    "postgres",
    "GigaChad1337"
  )

  def createPlayer(player: Player): IO[Unit] = {
    val createPlayer = sql"INSERT INTO players (login, password, amount) VALUES (${player.login}, ${player.password}, 100000)"
    createPlayer.update.run.transact(xa).attempt.flatMap {
      case Left(_) => IO.raiseError(new IllegalAccessError("Player exists"))
      case Right(_) => IO.unit
    }
  }

  def deletePlayer(login: Login): IO[Unit] = {
    val deletePlayer = sql"DELETE FROM players WHERE login = $login"
    deletePlayer.update.run.transact(xa).attempt.flatMap{
      case Left(_) => IO.raiseError(new IllegalAccessError("Player not exists"))
      case Right(_) => IO.unit
    }
  }

  def verifyPlayer(player: Player): IO[Unit] = for {
    either <- sql"SELECT password FROM players WHERE login = ${player.login.value}".query[Password].unique.transact(xa).attempt
    res <- either match {
      case Left(_) => IO.raiseError(new IllegalAccessError("Player not exists"))
      case Right(password) => if (password == player.password) IO.unit
      else IO.raiseError(new IllegalAccessError("Wrong password"))
    }
  } yield res

  def getBalance(login: Login): IO[Int] = {
    val query = sql"SELECT amount FROM players WHERE login = $login".query[Int]
    val action = query.unique
    action.transact(xa)
  }

  def updateBalance(value: Int, login: Login): IO[Int] = {
    val query = sql"UPDATE players SET amount = amount + $value WHERE login = $login"
    query.update.run.transact(xa)
  }

  def createBD: IO[Int] = {
    val create =
      sql"""
    CREATE TABLE IF NOT EXISTS players(
    login text NOT NULL,
    password text NOT NULL,
    amount INT CHECK (amount >= 0),
    UNIQUE(login)
);
  """.update.run

    create.transact(xa)
  }

  //создание БД
  val run: IO[Unit] = createBD.map(_ => ())
}
