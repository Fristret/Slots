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

object Routes {

  import MessageJson._
  import org.http4s.circe.CirceEntityCodec._

  // valid Registration:    curl -X POST -H "Content-Type:application/json" -d "{\"mail\":{\"mail\":\"d\"}, \"player\":{\"login\": {\"login\": \"masana\"},\"password\": {\"password\": \"mig943g\"}}}" http://localhost:9001/authorization

  // valid LogIn: curl -X POST -H "Content-Type:application/json" -d "{\"login\": {\"login\": \"masana\"},\"password\": {\"password\": \"mig943g\"}}" http://localhost:9001/authorization

  //websocat ws://127.0.0.1:9001/message/ token

  //вход Bet: {"amount": "10","multiple": "2"}
  //вход Balance: {"message": "balance"}

  def messageRoute(topic: Topic[IO, String], cache: Ref[IO, Map[Token, Instant]], ref: Ref[IO, Map[Mail, Player]]): HttpRoutes[IO] = {

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

      case req @ POST -> Root / "authorization" =>

        def tokenGenerator(player: Player): IO[Token] = {
          val id = Token(UUID.randomString ++ "&" ++ player.login.login)
          cache
            .modify { map =>
              (map + (id -> Instant.now().plus(5.toLong, ChronoUnit.MINUTES)), id)
            }
        }

        def createPlayer(player: NewPlayer): IO[Either[String, IO[Token]]] = {
          ref.modify {
            map =>
              map.get(player.mail) match {
                case None => (map + (player.mail -> player.player), Right(tokenGenerator(player.player)))
                case Some(x) => (map, Left(s"Player ${x.login} exists"))
              }
          }
        }

        def verifyPlayer(player: Player): IO[Either[String, IO[Token]]] = IO(???)

        {
          for {
            message <- req.as[MessageIn]
            either <- message match {
              case newPlayer: NewPlayer => newPlayer.player.checkSyntax *> createPlayer(newPlayer)
              case player: Player => player.checkSyntax *> verifyPlayer(player)
            }
            response <- either match {
              case Left(x) => BadRequest(s"Mistake: $x")
              case Right(a) => Ok(a.unsafeRunSync())
                }
            } yield response
        }.handleErrorWith(e => BadRequest(s"Error: ${e.getMessage}"))
    }
  }
}
