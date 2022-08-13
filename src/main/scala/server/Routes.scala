package server

import cats.effect._
import cats.effect.concurrent.Ref
import fs2.{Pipe, Stream}
import fs2.concurrent.Topic
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import server.models.Protocol._
import io.jvm.uuid._
import server.models.CommonClasses._

import java.time.Instant
import java.time.temporal.ChronoUnit
import server.utils.CheckSyntax._
import server.service.Doobie._
import cats.implicits.catsSyntaxSemigroup
import game.PRG.createNewStage
import game.Slot
import io.circe.Json
import io.circe.parser._
import io.circe.syntax.EncoderOps
import game.models.MiniGameObjects._

import scala.concurrent.ExecutionContext

object Routes {

  implicit private val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  import server.json.MessageJson._
  import org.http4s.circe.CirceEntityCodec._

  // valid Registration: curl -X POST -H "Content-Type:application/json" -d "{\"mail\":{\"value\":\"d\"}, \"player\":{\"login\": {\"value\": \"masanata23123\"},\"password\": {\"value\": \"mig943g\"}}}" http://localhost:9001/authorization

  // valid LogIn: curl -X POST -H "Content-Type:application/json" -d "{\"login\": {\"value\": \"masana232\"},\"password\": {\"value\": \"mig943g\"}}" http://localhost:9001/authorization

  // valid LogIn: curl -X POST -H "Content-Type:application/json" -d "{\"login\": {\"value\": \"masanata23123\"},\"password\": {\"value\": \"mig943g\"}}" http://localhost:9001/authorization

  //websocat ws://127.0.0.1:9001/message/"fc9a9be3-8830-4cc3-b909-c2eeef7d8b42&masana232"
  //websocat ws://127.0.0.1:9001/message/"ff63a28f-4d59-49bf-a143-84bd2e58c83f&masana23"
  //вход Bet: {"amount": "200"}
  //вход Balance: {"message": "balance"}

  def messageRoute(topicGlobal: TopicGlobal, cache: Cache, rpgProgress: RPGProgress): HttpRoutes[IO] = {

    HttpRoutes.of[IO] {
      case GET -> Root / "message" / id =>

        val token = Token(id)

        def verification: IO[Option[Login]] = cache.get.map(_.get(token)) map {
          _ => Some(Login(token.id.split("&").reverse.head))
        }

        def spinRepeat(f: => IO[SlotExit], times: Int,  topicClient: TopicClient, count: Int = 1): IO[Int] = f.flatMap(win => IO(win.value) |+| (if (count >= times) IO(0) else spinRepeat(f, times, topicClient, count + 1)))

        def doBet(bet: Bet, login: Login, topicClient: TopicClient, miniGameRef: MiniGameRef): IO[String] = {
          for {
            spirit <- miniGameRef.get
            slot = Slot(bet, login, topicClient, rpgProgress, spirit)
            _ <- updateBalance(-bet.amount, login)
            bal <- getBalance(login)
            _ <- topicClient.publish1(BalanceOut(bal).asJson.noSpaces)
            slotExit <- slot.spin
            winFree <- if (slotExit.freeSpins) spinRepeat(slot.spin, 10, topicClient)
                else IO(0)
            _ <- if (slotExit.value + winFree >= bet.amount * 5) topicGlobal.publish1(WinOutput(login.value, slotExit.value, Instant.now()).asJson.toString)
              else IO.unit
            _ <- updateBalance(slotExit.value + winFree, login)
            balanceAfterWin <- getBalance(login)
          } yield BalanceOut(balanceAfterWin).asJson.noSpaces
        }

        def changeSpirit(miniGameRef: MiniGameRef, spirit: MiniGameUnit): IO[Unit] =
          miniGameRef.update(_ => spirit)

        def toClient(topicClient: TopicClient): Stream[IO, WebSocketFrame] = {
          val stream1 = topicGlobal
            .subscribe(10)
            .map(WebSocketFrame.Text(_))
          val stream2 = topicClient.subscribe(30).map(WebSocketFrame.Text(_))
          Stream(stream2, stream1).parJoinUnbounded
        }

        def fromClient(login: Login, topicClient: TopicClient, miniGameRef: MiniGameRef): Pipe[IO, WebSocketFrame, Unit] = topicClient
          .publish
          .compose[Stream[IO, WebSocketFrame]](_.collect {
            case WebSocketFrame.Text(message, _) =>
              val json = parse(message) match {
                case Right(value) => value
                case Left(_) => Json.Null
              }
              val messageParsed = json.as[MessageIn] match {
                case Right(x) => IO(x)
                case Left(_) => IO.raiseError(new IllegalAccessError("Wrong message"))
              }
              messageParsed.flatMap{
                  case bet: Bet => bet.checkSyntax match {
                    case Right(_) => doBet(bet, login, topicClient, miniGameRef)
                    case Left(err) => IO.raiseError(new IllegalStateException(err))
                  }
                  case option: MiniGameOption => changeSpirit(miniGameRef, option.spirit).flatMap(_ => miniGameRef.get.map(x => MiniGameOption(x).asJson.noSpaces))
                  case _ => IO.pure("")
                }.handleErrorWith(e => IO(ErrorMessage(s"${e.getMessage}").asJson.noSpaces)).unsafeRunSync()
          }
          )

        for {
          topicClient <- Topic[IO, String]("Client")
          res <- verification.flatMap {
            case None => BadRequest()
            case Some(name) => for {
              miniGameRef <- Ref[IO].of[MiniGameUnit](Leaf)
              _ <- rpgProgress.modify{
                    map => map.get(name) match {
                      case None => (map + (name -> createNewStage), map)
                      case Some(_) => (map, map)
                    }
                  }
              _ <- cache.modify(map => (map.removed(token), ()))
              response <- WebSocketBuilder[IO].build(
                receive = fromClient(name, topicClient, miniGameRef),
                send = toClient(topicClient)
              )
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
                case Right(_) => createPlayer(newPlayer.player) *> Ok()
                case Left(err) => BadRequest(ErrorMessage(err))
              }
              case player: Player => player.checkSyntax match {
                case Right(_) => verifyPlayer(player) *> Ok(tokenGenerator(player))
                case Left(err) => BadRequest(ErrorMessage(err))
              }
              case _ => BadRequest(ErrorMessage("Wrong message"))
            }
            } yield resp
        }.handleErrorWith(e => BadRequest(ErrorMessage(s"${e.getMessage}")))
    }
  }
}
