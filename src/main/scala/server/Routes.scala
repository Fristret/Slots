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
import CheckSyntax._
import Doobie._

object Routes {

  import MessageJson._
  import org.http4s.circe.CirceEntityCodec._

  // valid Registration: curl -X POST -H "Content-Type:application/json" -d "{\"mail\":{\"mail\":\"d\"}, \"player\":{\"login\": {\"login\": \"masana\"},\"password\": {\"password\": \"mig943g\"}}}" http://localhost:9001/authorization

  // valid LogIn: curl -X POST -H "Content-Type:application/json" -d "{\"login\": {\"login\": \"masana\"},\"password\": {\"password\": \"mig943g\"}}" http://localhost:9001/authorization

  //websocat ws://127.0.0.1:9001/message/ token
  //websocat ws://127.0.0.1:9001/message/"5fa5ac97-0953-41a9-acf6-8f8f574bd2ba&masana"
  //вход Bet: {"amount": "20"}
  //вход Balance: {"message": "balance"}

  def messageRoute(topic: Topic[IO, String], cache: Ref[IO, Map[Token, Instant]]): HttpRoutes[IO] = {

    HttpRoutes.of[IO] {
      case GET -> Root / "message" / id =>

        val token = Token(id)

        def verification: IO[Option[Login]] = cache.get.map(_.get(token)) map {
          _ => Some(Login(token.id.split("&").reverse.head))
        }

        //написать Игру
        def doBet(bet: Bet, login: Login): IO[String] = for {
          win <- updateBalance( - bet.amount, login) *> IO(Win(20))
          res <- IO(s"You $win")
          _ <- updateBalance(win.win, login)
          bal2 <- getBalance(login)
          _ <- topic.publish1(bal2)
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

      case req @ POST -> Root / "authorization" =>

        def tokenGenerator(player: Player): IO[Token] = {
          val id = Token(UUID.randomString ++ "&" ++ player.login.login)
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
