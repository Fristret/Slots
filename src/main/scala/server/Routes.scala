package server

import cats.effect._
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

import java.time.Instant
import java.time.temporal.ChronoUnit
import CheckSyntax._
import Doobie._
import Game.RPG.createNewRPG
import Game.RPGElements.Stage
import cats.implicits.catsSyntaxSemigroup
import io.circe.syntax.EncoderOps

import scala.concurrent.ExecutionContext

object Routes {

  implicit private val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  import MessageJson._
  import org.http4s.circe.CirceEntityCodec._

  // valid Registration: curl -X POST -H "Content-Type:application/json" -d "{\"mail\":{\"value\":\"d\"}, \"player\":{\"login\": {\"value\": \"masana23123\"},\"password\": {\"value\": \"mig943g\"}}}" http://localhost:9001/authorization

  // valid LogIn: curl -X POST -H "Content-Type:application/json" -d "{\"login\": {\"value\": \"masana23123\"},\"password\": {\"value\": \"mig943g\"}}" http://localhost:9001/authorization

  // valid LogIn: curl -X POST -H "Content-Type:application/json" -d "{\"login\": {\"value\": \"masana23\"},\"password\": {\"value\": \"mig943g\"}}" http://localhost:9001/authorization

  //websocat ws://127.0.0.1:9001/message/"60900fe2-dae9-4647-ad1d-0252581a651b&masana23"
  //websocat ws://127.0.0.1:9001/message/"39e5e3ea-a734-4a8e-a6bc-85b65edc83c7&masana23"
  //вход Bet: {"amount": "200"}
  //вход Balance: {"message": "balance"}

  def messageRoute(topic: Topic[IO, String], cache: Ref[IO, Map[Token, Instant]], rpgProgress: Ref[IO, Map[Login, Stage]]): HttpRoutes[IO] = {

    HttpRoutes.of[IO] {
      case GET -> Root / "message" / id =>

        val token = Token(id)

        def verification: IO[Option[Login]] = cache.get.map(_.get(token)) map {
          _ => Some(Login(token.id.split("&").reverse.head))
        }

        def spinRepeat(f: => IO[Win], times: Int,  topicClient: Topic[IO, String], count: Int = 1): IO[Int] = {for {
          win <- f
          _ <- topicClient.publish1(win.asJson.noSpaces)
        } yield win.value} |+| (if (count >= times) IO(0) else spinRepeat(f, times, topicClient, count + 1))

        def doBet(bet: Bet, login: Login, topicClient: Topic[IO, String]): IO[String] = {
          for {
            _ <- updateBalance(-bet.amount, login)
            bal <- getBalance(login)
            _ <- topicClient.publish1(BalanceOut(bal).asJson.noSpaces)
            win <- spin(bet, login, rpgProgress)
            _ <- topicClient.publish1(win.asJson.noSpaces)
            winFree <- if (win.freeSpins) spinRepeat(spin(bet, login, rpgProgress), 10, topicClient)
                else IO(0)
            _ <- if (win.value + winFree >= 10000) topic.publish1(WinOutput(login.value, win.value, Instant.now()).asJson.toString)
              else IO.unit
            _ <- updateBalance(win.value + winFree, login)
            balanceAfterWin <- getBalance(login)
          } yield BalanceOut(balanceAfterWin).asJson.noSpaces
        }.handleErrorWith(e => IO(s"${e.getMessage}"))

        def toClient(topic2: Topic[IO, String]): Stream[IO, WebSocketFrame] = {
          val stream1 = topic
            .subscribe(10)
            .map(WebSocketFrame.Text(_))
          val stream2 = topic2.subscribe(10).map(WebSocketFrame.Text(_))
          stream2 ++ stream1
        }

        def fromClient(login: Login, topic2: Topic[IO, String]): Pipe[IO, WebSocketFrame, Unit] = topic2
          .publish
          .compose[Stream[IO, WebSocketFrame]](_.collect {
            case WebSocketFrame.Text(message, _) => {
              for {
                json <- parse(message)
                message <- json.as[MessageIn]
                result = message match {
                  case bet: Bet => bet.checkSyntax match {
                    case Right(_) => doBet(bet, login, topic2)
                    case Left(err) => IO.raiseError(new IllegalStateException(err))
                  }
                  case _: Balance => getBalance(login).map(_.toString).handleErrorWith(e => IO(s"${e.getMessage}"))
                  case _ => IO.pure("")
                }
              } yield result.handleErrorWith(e => IO(s"${e.getMessage}")).unsafeRunSync()
            }.getOrElse("Wrong message")
          }
          )

        for {
          topicClient <- Topic[IO, String]("")
          res <- verification.flatMap {
            case None => BadRequest()
            case Some(name) => for {
              _ <- createNewRPG(name, rpgProgress)
              str <- cache.modify(map => (map.removed(token), s"Welcome, ${name.value}"))
              _ <- topicClient.publish1(str)
              response <- WebSocketBuilder[IO].build(receive = fromClient(name, topicClient), send = toClient(topicClient))
            } yield response
          }
        } yield res

      case req @ POST -> Root / "authorization" =>

        def tokenGenerator(player: Player): IO[Token] = {
          val id = Token(UUID.randomString ++ "&" ++ player.login.value)
          cache
            .modify { map =>
              (map + (id -> Instant.now().plus(2.toLong, ChronoUnit.MINUTES)), id)
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
        }.handleErrorWith(e => BadRequest(s"${e.getMessage}"))
    }
  }
}
