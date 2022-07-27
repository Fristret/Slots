package server

import cats.effect.{ExitCode, IO, IOApp}
import doobie.util.transactor.Transactor
import doobie.postgres.circe._
import doobie.postgres.circe.json.implicits._
import Protocol._
import doobie.implicits._
import io.circe.parser._

object Doobie extends IOApp {

  implicit class Debugger[A](io: IO[A]) {
    def debug: IO[A] = io.map {
      a =>
        println(s"[${Thread.currentThread().getName}] $a")
        a
    }
  }

  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql://localhost:5432/test",
    "postgres",
    "GigaChad1337"
  )

//  def findAllPlayers: IO[List[Json]] = {
//    val query = sql"SELECT * FROM players".query[Json]
//    val action = query.map(a => parse(a)).to[List]
//    action.transact(xa)
//  }


  override def run(args: List[String]): IO[ExitCode] = ???
//    findAllPlayers.debug.as(ExitCode.Success)
}
