package server

import fs2.{Pull, Stream}
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.toSemigroupKOps
import fs2.concurrent._
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
// Complete all TODOs below according to their description.


object Protocol {
  trait Message
  case class Login(login: String) extends Message
  case class Balance(amountBalance: BigDecimal)
  case class Combination(comb: List[Int])
  case class Player(login: Login, password: String, balance: Balance)
  case class Bet(login: Login, amountBet: Int, multiple: Int)
  case class Win(win: BigDecimal)
  case class WinOutput(login: Login, amountWin: Win, time: Instant)
}

object SlotServer extends IOApp {

  import Protocol._
  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._

  //curl "localhost:9001/connection"
  private val connectionRoute = HttpRoutes.of[IO] {
    case GET -> Root / "connection" => Ok(s"Суксессфуль коннектионе")
  }

  private def balanceRoute(topic: Topic[IO, String]) = HttpRoutes.of[IO] {
    case GET -> Root / "message" =>
      WebSocketBuilder[IO].build(
        receive = topic.publish.compose[Stream[IO, WebSocketFrame]](_.collect {
          case WebSocketFrame.Text(message, _) => message
        }.pull
          .uncons1
          .flatMap {
            case None => Pull.done
            case Some((message1, stream)) if message1.isEmpty => Pull.done
            case Some((message1, stream)) => stream.map(message => s"$message1: $message").pull.echo
          }
          .stream
        ),
        send = topic.subscribe(3).map(WebSocketFrame.Text(_)),
      )
  }




//      for {
//        response <- WebSocketBuilder[IO].build(
//          receive = topic.publish.compose[Stream[IO, WebSocketFrame]](
//            _
//              .collect {
//                case WebSocketFrame.Text(_, _) => None
//              }),
//          send = topic.subscribe(3).filter(_.nonEmpty).map {
//            case Some(balance) => WebSocketFrame.Text(balance.asJson.noSpaces)
//            case _ => WebSocketFrame.Text("Error")
//          }
//        )
//      } yield response
//      )
//  }

  private def userIdentifierRoute(topic: Topic[IO, String]) =
    for {
      ref <- Ref[IO].of(Map.empty[String, String])
    } yield HttpRoutes.of[IO] {

      //curl -X POST "localhost:9001/user" -d '{"login": "masana", "password": "123fdsf1315"}'
      case req @ POST -> Root / "player" =>

        def verifyPlayer(player: Player): IO[String] = {
          for {
            playerReal <- ref.modify { playerRef =>
              val verify = playerRef.get(player.login.login)
              verify match {
                case None => (playerRef + (player.login.login -> player.password), s"Пользователь создан")
                case Some(password) => (playerRef, s"Добро пожаловать, ${player.login.login}")
              }
            }
            _ <- topic.publish1(playerReal)
          } yield playerReal
        }

        req.as[Player].flatMap { player =>
            Ok(verifyPlayer(player))
        }.handleErrorWith(e => BadRequest(s"Ха, лох, ${e.getMessage}"))
    }

  private[server] def httpApp(topic: Topic[IO, String]) = for {
    user <- userIdentifierRoute(topic)
  } yield Seq(user,
    balanceRoute(topic),
    connectionRoute,
  ).reduce(_ <+> _)
    .orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      topic <- Topic[IO, String]("Welcome")
      httpAp <- httpApp(topic)
      _ <- BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withHttpApp(httpAp)
        .serve
        .compile
        .drain
    }yield ExitCode.Success
}

}
//object WebSocketClient extends IOApp {
//
//  private val uri = uri"ws://localhost:9002/chat"
//
//  private def printLine(string: String = ""): IO[Unit] = IO(println(string))
//
//  override def run(args: List[String]): IO[ExitCode] = {
//    val clientResource = Resource.eval(IO(HttpClient.newHttpClient()))
//      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))
//
//    clientResource.use { client =>
//      for {
//        _ <- client.send(WSFrame.Text("Hello, world!"))
//        _ <- client.receiveStream.collectFirst {
//          case WSFrame.Text(s, _) => s
//        }.compile.string >>= printLine
//      } yield ExitCode.Success
//    }
//  }
//}

//        def updateBalance(transaction: Transaction): IO[Json] = {
//          for {
//            newBalance <- ref.modify { balance =>
//              val oldAmount: BigDecimal = balance.get(transaction.playerId) match {
//                case Some(Balance(playerId, oldAmount, updatedAt)) => oldAmount
//                case None => 0
//              }
//              val newAmount: BigDecimal = oldAmount + transaction.deltaAmount
//              val newBalance = Balance(transaction.playerId, newAmount, Instant.now())
//              (balance + (transaction.playerId -> newBalance), newBalance)
//            }
//            _ <- topic.publish1(Some(newBalance))
//          } yield newBalance.asJson
//        }
//
//        req.as[Transaction].flatMap {
//          transaction =>
//            Accepted(updateBalance(transaction))
//        }.handleErrorWith(e => BadRequest(e.getMessage))

//      case GET -> Root / "balance" =>
//        for {
//          response <- WebSocketBuilder[IO].build(
//            receive = topic.publish.compose[Stream[IO, WebSocketFrame]](
//              _
//                .collect {
//                  case WebSocketFrame.Text(_, _) => None
//                }),
//            send = topic.subscribe(3).filter(_.nonEmpty).map {
//              case Some(balance) => WebSocketFrame.Text(balance.asJson.noSpaces)
//              case _ => WebSocketFrame.Text("Error")
//            }
//          )
//        } yield response
//    }.orNotFound
// }

//  override def run(args: List[String]): IO[ExitCode] =
//    for {
//      httpApp <- httpApp
//      _ <- BlazeServerBuilder[IO](ExecutionContext.global)
//        .bindHttp(port = 9001, host = "localhost")
//        .withHttpApp(httpApp)
//        .serve
//        .compile
//        .drain
//    } yield ExitCode.Success