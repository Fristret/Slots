package server

import cats.effect.IO
import cats.effect.concurrent.Ref
import fs2.{Pipe, Stream}
import fs2.concurrent.Topic
import io.circe.parser.parse
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import server.Protocol._
import io.jvm.uuid._
import server.CommonClasses.Token

import java.time.Instant
import java.time.temporal.ChronoUnit

object Routes {

  import MessageJson._
  import org.http4s.circe.CirceEntityCodec._

  // valid:    curl -X POST -H "Content-Type:application/json" -d "{\"message\":\"NewPlayer\",\"login\":\"masana\",\"password\":\"mig943g\"}" http://localhost:9001/registration

  // invalid: curl -X POST -H "Content-Type:application/json" -d "{\"message\":\"LogIn\",\"login\":\"masana\",\"password\":\"mig943\"}" http://localhost:9001/registration

  //  invalid:  curl -X POST -H "Content-Type:application/json" -d "{\"message\":\"NewPlayer\",\"login\":\"abobaoboaodofosfcgvbnm__==sdad\",\"password\":\"mig943g\"}" http://localhost:9001/registration

  //  curl -X POST -H "Content-Type:application/json" -d "{\"message\":\"LogIn\",\"login\":\"masana\",\"password\":\"mig943g\"}" http://localhost:9001/registration

  //websocat ws://127.0.0.1:9001/message/"3c6de2d5-c54f-4514-9f22-1c0c7ab87a79&masana"

  //вход Bet: {"message": "bet","amount": "10","multiple": "2"}
  //вход Balance: {"message": "balance"}

  def messageRoute(topic: Topic[IO, String], cache: Ref[IO, Map[Token, Instant]]): HttpRoutes[IO] = {

    HttpRoutes.of[IO] {
      case GET -> Root / "message" / id =>

        val token = Token(id)

        def verification: IO[Option[String]] = cache.get.map(_.get(token)).map{
          case Some(value) => Some(token.id.split("&").reverse.head)
        }

        //написать Игру
        def doBet(bet: Bet, login: String): IO[String] = IO("Your bet")

        //разобраться с Doobie
        def getBalance(login: String) = IO("Your balance")

        def toClient: Stream[IO, WebSocketFrame] = topic
          .subscribe(3)
          .map(WebSocketFrame.Text(_))

        def fromClient(login: String): Pipe[IO, WebSocketFrame, Unit] = topic
          .publish
          .compose[Stream[IO, WebSocketFrame]](_.collect {
            case WebSocketFrame.Text(message, _) => {
              for {
                json <- parse(message)
                message <- json.as[MessageIn]
                result = message match {
                  case bet: Bet => doBet(bet, login)
                  case _: Balance => getBalance(login)
                  case _ => IO.pure("try better")
                }
              } yield result.unsafeRunSync()
            }.getOrElse("Wrong")
          }
          )

        for {
          res <- verification.flatMap {
            case None => BadRequest()
            case Some(x) => for {
              _ <- cache.modify(map => (map.removed(token), s"Welcome, $x"))
              _ <- topic.publish1(
                """|Выберите функцию:
                   |1. Сделать ставку (bet)
                   |2. Посмотреть баланс (balance)
                   |""".stripMargin)
              response <- WebSocketBuilder[IO].build(receive = fromClient(x), send = toClient)
            } yield response
          }
          } yield res
    }
  }

  def registrationRoute(ref: Ref[IO, Map[String, Player]], cache: Ref[IO, Map[Token, Instant]]): HttpRoutes[IO] = {

    HttpRoutes.of[IO] {

      case req @ POST -> Root / "registration" =>

        def tokenGenerator(player: Player): IO[Token] = {
          val id = Token(UUID.randomString ++ "&" ++ player.login)
          cache
            .modify { map =>
              (map + (id -> Instant.now().plus(5.toLong, ChronoUnit.MINUTES)), id)
            }

        }

        def checkPlayerSyntax(player: Player): IO[String] = {
          loginCheck(player.login).handleErrorWith(err => IO(s"Error: $err")) *> passwordCheck(player.password).handleErrorWith(err => IO(s"Error: $err"))
        }

        def loginCheck(login: String): IO[String] = if (login.length >= 3 && login.length <= 15 && login.matches("[a-zA-Z0-9]+")) IO(login)
          else IO.raiseError(new IllegalStateException("Wrong login"))

        def passwordCheck(password: String): IO[String] = if (password.length >= 6 && password.length <= 15 && password.matches("[a-zA-Z0-9]+")) IO(password)
        else IO.raiseError(new IllegalStateException("Wrong password"))

        def registration(player: Player): IO[Either[String, IO[Token]]] = {
          player.message match {
            case "NewPlayer" => createPlayer(player)
            case "LogIn" => verifyPlayer(player)
            case _ => IO(Left("Wrong message"))
          }
        }

        def createPlayer(player: Player): IO[Either[String, IO[Token]]] = {
          ref.modify {
            map =>
              map.get(player.login) match {
                case None => (map + (player.login -> player), Right(tokenGenerator(player)))
                case Some(x) => (map, Left(s"Player ${x.login} exists"))
              }
          }
        }

        def verifyPlayer(player: Player): IO[Either[String, IO[Token]]] = {
          ref.modify {
            map =>
              map.get(player.login) match {
                case None => (map, Left(s"Player ${player.login} don't exist"))
                case Some(x) => if (x.password == player.password) (map, Right(tokenGenerator(player)))
                else (map, Left(s"Wrong password"))
              }
          }
        }

        req.as[Player]
          .flatMap(player => for {
            either <- checkPlayerSyntax(player) *> registration(player)
            token <- either match {
              case Left(x) => BadRequest(s"Mistake: $x")
              case Right(a) => Ok(a.unsafeRunSync())
            }
          } yield token
          )
          .handleErrorWith(e => BadRequest(s"Error: ${e.getMessage}"))
    }
  }
}
