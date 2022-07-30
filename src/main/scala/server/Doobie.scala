package server

import cats.effect._
import doobie.util.transactor.Transactor
import Protocol._
import doobie.implicits._

object Doobie extends IOApp.Simple {

  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql://localhost:5432/SlotDB",
    "postgres",
    "GigaChad1337"
  )

  def createPlayer(player: Player): IO[String] = {
    val createPlayer = sql"INSERT INTO players (login, password, amount) VALUES (${player.login}, ${player.password}, 100000)"
    createPlayer.update.run.transact(xa).attempt.flatMap{
      case Left(error) => IO.raiseError(new IllegalAccessError("Player exists"))
      case Right(value) => IO("Good")
    }
  }

  def verifyPlayer(player: Player): IO[Unit] = for {
    either <- sql"SELECT password FROM players WHERE login = ${player.login}".query[Password].unique.transact(xa).attempt
    res <- either match {
      case Left(err) => IO.raiseError(err)
      case Right(password) => if (password == player.password) IO.unit
        else IO.raiseError(new IllegalAccessError("Wrong password"))
    }
  } yield res

  def getBalance(login: Login): IO[String] = {
    val query = sql"SELECT amount FROM players WHERE login = $login".query[String]
    val action = query.unique
    action.transact(xa)
  }

  def updateBalance(value: Int, login: Login): IO[Int]  ={
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
  val run: IO[Unit] = for {
    pr <- IO(println("Hello, World"))
    _ <- createBD
  } yield pr
}
