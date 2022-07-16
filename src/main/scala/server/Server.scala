package server

import fs2.{Pipe, Pull, Stream}
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.toSemigroupKOps
import fs2.concurrent._
import io.circe.parser._
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.http4s.HttpRoutes
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import java.time.Instant
import scala.concurrent.ExecutionContext
import Protocol._
// Complete all TODOs below according to their description.

object Server extends IOApp {

  import MessageJson._
  import org.http4s.circe.CirceEntityCodec._

  //websocat ws://127.0.0.1:9001/message
  private def messageRoute(topic: Topic[IO, String], ref: Ref[IO, Map[String, Player]]): HttpRoutes[IO] = {

    //Я думаю, что методы можно в отдельный объект переместить и оставить тут только Route

    def doBet(bet: Bet): String = s"Correct Bet" //написать Игру

    def getBalance = s"Your Balance" //разобраться с Doobie

    def registration(player: Player): IO[String] = {
      player.message.message match {
        case "NewPlayer" => createPlayer(player)
        case "LogIn" => verifyPlayer(player)
      }
    }

    def createPlayer(player: Player): IO[String] = {
      ref.modify {
        map =>
          map.get(player.login) match {
            case None => (map + (player.login -> player), s"Welcome, ${player.login}")
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


    HttpRoutes.of[IO] {
      case req@POST -> Root / "message" =>

        def toClient: Stream[IO, WebSocketFrame] = topic
          .subscribe(3)
          .map(WebSocketFrame.Text(_))

        //вход Bet: {"message": "bet", "login": "player123", "password": "dddd"}
        //вход Balance: {"message": "balance"}

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
                  } yield doBet(bet)
                  case "balance" => Right(getBalance)
                  case _ => Left("try better")
                }
              } yield result
            }.getOrElse("Wrong")
          }
          )

        //вход NewPlayer: {"message": "NewPlayer", "login": "player123", "password": "dddd"}
        //вход LogIn: {"message": "LogIn", "login": "player123", "password": "dddd"}


        val playerReq = req.as[Player]
        playerReq.flatMap(player => Ok(registration(player))).handleErrorWith(e => BadRequest(e.getMessage)) match {
          case Ok =>
            for {
              _ <- topic.publish1(
                """|Выберите функцию:
                   |1. Сделать ставку (bet)
                   |2. Посмотреть баланс (balance)
                   |""".stripMargin)
              response <- WebSocketBuilder[IO].build(receive = fromClient, send = toClient)
            } yield response
          case BadRequest => BadRequest("Wrong body")
        }
    }
  }

  private[server] def httpApp(topic: Topic[IO, String], ref: Ref[IO, Map[String, Player]]) = {
    messageRoute(topic, ref)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      ref <- Ref[IO].of(Map.empty[String, Player])
      topic <- Topic[IO, String]("Welcome. Write your request")
      _ <- BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withHttpApp(httpApp(topic, ref))
        .serve
        .compile
        .drain
    } yield ExitCode.Success
  }
}