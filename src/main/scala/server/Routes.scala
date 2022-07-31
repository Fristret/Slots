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
import Game.Slot._
import Game.RPGElements._

import java.time.Instant
import java.time.temporal.ChronoUnit
import CheckSyntax._
import Doobie._
import Game.RPG.createNewRPG
import Game.RPGElements.Stage
import io.circe.syntax.EncoderOps

object Routes {

  import MessageJson._
  import org.http4s.circe.CirceEntityCodec._

  // valid Registration: curl -X POST -H "Content-Type:application/json" -d "{\"mail\":{\"value\":\"d\"}, \"player\":{\"login\": {\"value\": \"masana23\"},\"password\": {\"value\": \"mig943g\"}}}" http://localhost:9001/authorization

  // valid LogIn: curl -X POST -H "Content-Type:application/json" -d "{\"login\": {\"value\": \"masana23\"},\"password\": {\"value\": \"mig943g\"}}" http://localhost:9001/authorization

  //websocat ws://127.0.0.1:9001/message/ token
  //websocat ws://127.0.0.1:9001/message/"8c9b8f21-414d-476b-b034-0d62e618c66b&masana23"
  //вход Bet: {"amount": "200"}
  //вход Balance: {"message": "balance"}

  def messageRoute(topic: Topic[IO, String], cache: Ref[IO, Map[Token, Instant]], rpgProgress: Ref[IO, Map[Login, Stage]]): HttpRoutes[IO] = {

    HttpRoutes.of[IO] {
      case GET -> Root / "message" / id =>

        val token = Token(id)

        def verification: IO[Option[Login]] = cache.get.map(_.get(token)) map {
          _ => Some(Login(token.id.split("&").reverse.head))
        }

        def doBet(bet: Bet, login: Login): IO[String] = for {
          win <- updateBalance( - bet.amount, login).handleErrorWith(_ => IO(s"You haven't money to Bet")) *> spin(bet, login, rpgProgress)
          res <- IO(s"Your $win")
          _ <- topic.publish1(WinOutput(login.value, win.value, Instant.now()).asJson.toString)
          _ <- updateBalance(win.value, login)
        } yield res

        def toClient: Stream[IO, WebSocketFrame] = topic
          .subscribe(3)
          .map(WebSocketFrame.Text(_))

        def fromClient(login: Login): Pipe[IO, WebSocketFrame, Unit] = topic
          .publish
          .compose[Stream[IO, WebSocketFrame]](_.collect {
            case WebSocketFrame.Text(message, _) => {
              for {
                json <- parse(message)
                message <- json.as[MessageIn]
                result = message match {
                  case bet: Bet => bet.checkSyntax match {
                    case Right(_) => doBet(bet, login)
                    case Left(err) => IO.raiseError(new IllegalStateException(err))
                  }
                  case _: Balance => getBalance(login).handleErrorWith(e => IO(s"Error: ${e.getMessage}"))
                  case _ => IO.pure("try better")
                }
              } yield result.handleErrorWith(e => IO(s"Error: ${e.getMessage}")).unsafeRunSync()
            }.getOrElse("Wrong")
          }
          )

        for {
          res <- verification.flatMap {
            case None => BadRequest()
            case Some(name) => for {
              _ <- createNewRPG(name, rpgProgress)
              _ <- cache.modify(map => (map.removed(token), s"Welcome, $name"))
              _ <- topic.publish1(
                """|Выберите функцию:
                   |1. Сделать ставку (bet)
                   |2. Посмотреть баланс (balance)
                   |""".stripMargin)
              response <- WebSocketBuilder[IO].build(receive = fromClient(name), send = toClient)
            } yield response
          }
        } yield res

      case req @ POST -> Root / "authorization" =>

        def tokenGenerator(player: Player): IO[Token] = {
          val id = Token(UUID.randomString ++ "&" ++ player.login.value)
          cache
            .modify { map =>
              (map + (id -> Instant.now().plus(5.toLong, ChronoUnit.MINUTES)), id)
            }
        }

        {
          for {
            message <- req.as[MessageIn]
            resp <- message match {
              case newPlayer: NewPlayer => newPlayer.player.checkSyntax match {
                case Right(_) => createPlayer(newPlayer.player) *> Ok("Player has been created")
                case Left(err) => BadRequest(err)
              }
              case player: Player => player.checkSyntax match {
                case Right(_) => verifyPlayer(player) *> Ok(tokenGenerator(player))
                case Left(err) => BadRequest(err)
              }
            }
            } yield resp
        }.handleErrorWith(e => BadRequest(s"Error: ${e.getMessage}"))
    }
  }
}
