package server

import cats.implicits._
import cats.effect.IO
import cats.effect.concurrent.Ref
import fs2.{Pipe, Stream}
import fs2.concurrent.Topic
import io.circe.parser.parse
import org.http4s.{EmptyBody, HttpRoutes}
import org.http4s.dsl.io._
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import server.Protocol._

object Routes {

  import MessageJson._
  import org.http4s.circe.CirceEntityCodec._

  // valid:    curl -X POST -H "Content-Type:application/json" -d "{\"message\":\"NewPlayer\",\"login\":\"masana\",\"password\":\"mig943g\"}" http://localhost:9001/registration

  //  invalid:  curl -X POST -H "Content-Type:application/json" -d "{\"message\":\"NewPlayer\",\"login\":\"abobaoboaodofosfcgvbnm__==sdad\",\"password\":\"mig943g\"}" http://localhost:9001/registration

  //  curl -X POST -H "Content-Type:application/json" -d "{\"message\":\"LogIn\",\"login\":\"masana\",\"password\":\"mig943g\"}" http://localhost:9001/registration

  //websocat ws://127.0.0.1:9001/message/masana

  //вход Bet: {"message": "bet","amount": "10","multiple": "2"}
  //вход Balance: {"message": "balance"}

  def messageRoute(topic: Topic[IO, String],ref: Ref[IO, Map[String, Player]]): HttpRoutes[IO] = {

    HttpRoutes.of[IO] {
      case GET -> Root / "message" / name =>
        //Я думаю, что методы можно в отдельный объект переместить и оставить тут только Route

        def verification(login: String) = ref modify{
          map => map.get(login) match {
            case None => (map, false)
            case Some(x) => (map, true)
          }
        }

        //написать Игру
        def doBet(bet: Bet, login: String): IO[String] = for {
          bool <- verification(login)
          res <- IO(if (bool) s"Correct bet"
          else s"No player")
        } yield res


        //разобраться с Doobie
        def getBalance(login: String) = for {
          bool <- verification(login)
          res <- IO(if (bool) s"Your balance"
          else s"No player")
        } yield res

        def toClient: Stream[IO, WebSocketFrame] = topic
          .subscribe(3)
          .map(WebSocketFrame.Text(_))

        def fromClient: Pipe[IO, WebSocketFrame, Unit] = topic
          .publish
          .compose[Stream[IO, WebSocketFrame]](_.collect {
            case WebSocketFrame.Text(message, _) => {
              for {
                json <- parse(message)
                message <- json.as[Message]
                result <- message.message match {
                  case "bet" => for {
                    bet <- json.as[Bet]
                  } yield doBet(bet, name)
                  case "balance" => Right(getBalance(name))
                  case _ => Left("try better")
                }
              } yield result.unsafeRunSync()
            }.getOrElse("Wrong")
          }
          )

        for {
          _ <- topic.publish1(
            """|Выберите функцию:
               |1. Сделать ставку (bet)
               |2. Посмотреть баланс (balance)
               |""".stripMargin)
          response <- WebSocketBuilder[IO].build(receive = fromClient, send = toClient)
        } yield response
    }
  }

  def registrationRoute(ref: Ref[IO, Map[String, Player]]): HttpRoutes[IO] = {

    HttpRoutes.of[IO] {

      case req @ POST -> Root / "registration" =>

        def checkPlayerSyntax(player: Player): IO[String] = {
          loginCheck(player.login) *> passwordCheck(player.password)
        }

        def loginCheck(login: String): IO[String] = if (login.length >= 3 && login.length <= 15 && login.matches("[a-zA-Z0-9]")) IO(login)
        else throw new Exception("Wrong login syntax")

        def passwordCheck(password: String): IO[String] = if (password.length >= 6 && password.length <= 15 && password.matches("[a-zA-Z0-9]")) IO(password)
        else throw new Exception("Wrong password syntax")

        def registration(player: Player): IO[String] = {
          player.message match {
            case "NewPlayer" => createPlayer(player)
            case "LogIn" => verifyPlayer(player)
          }
        }

        def createPlayer(player: Player): IO[String] = {
          ref.modify {
            map =>
              map.get(player.login) match {
                case None => (map + (player.login -> player), s"You have born, ${player.login}")
                case Some(x) => (map, s"Player ${x.login} exists")
              }
          }
        }

        def verifyPlayer(player: Player): IO[String] = {
          ref.modify {
            map =>
              map.get(player.login) match {
                case None => (map, s"Player ${player.login} don't exist")
                case Some(x) => if (x.password == player.password) (map, s"Welcome, ${player.login}")
                else (map, s"Wrong password")
              }
          }
        }

        req.as[Player].flatMap(player => Ok(checkPlayerSyntax(player) *> registration(player))).handleErrorWith(e => BadRequest(s"Error: ${e.getMessage}"))
    }
  }
}
