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

  //curl "localhost:9001/connection"
  private val connectionRoute = HttpRoutes.of[IO] {
    case GET -> Root / "connection" => Ok(s"Суксессфуль коннектионе")
  }

  //websocat "ws://127.0.0.1:9001/message"
  private def messageRoute(topic: Topic[IO, String], ref: Ref[IO, Map[String, Player]]): HttpRoutes[IO] = {

    def doBet(bet: Bet): String = s"Correct Bet"
    def getBalance = s"Your Balance"
    def createPlayer(player: Player): String = s"Player created"
    def verifyPlayer(player: Player): String = s"Hello, ${player.login}"





    HttpRoutes.of[IO] {
      case GET -> Root / "message" =>

        def toClient: Stream[IO, WebSocketFrame] = topic
          .subscribe(3)
          .map(WebSocketFrame.Text(_))

        //вход Balance: balance
        //вход Bet: bet {"amountBet": 20, "multiple": 5}
        //вход newPLayer: newPlayer {"login": "player123", "password": "dddd"}
        //вход logIn: logIn {"login": "player123", "password": "dddd"}

        def fromClient: Pipe[IO, WebSocketFrame, Unit] = topic
          .publish
          .compose[Stream[IO, WebSocketFrame]](_.collect {
            case WebSocketFrame.Text(message, _) => message.trim.split(" ").head match {
              case "bet" => parse(message.split("bet").mkString.trim).flatMap(_.as[Bet]).flatMap(bet => Right(doBet(bet))) match {
                case Right(value) => value
                case Left(value) => s"Cringe $value"
              }
              case "balance" => getBalance
              case "newPlayer" => parse(message.split("newPlayer", 2).mkString.trim).flatMap(_.as[Player]).flatMap(player => Right(createPlayer(player))) match {
                case Right(value) => value
                case Left(value) => s"Cringe $value"
              }
              case "logIn" => parse(message.split("logIn", 2).mkString.trim).flatMap(_.as[Player]).flatMap(player => Right(verifyPlayer(player)))match {
                case Right(value) => value
                case Left(value) => s"Cringe $value"
              }
              case _ => "try better"
            }
          }
          )



        for {
          _ <- topic.publish1("""|Выберите функцию:
            |1. Сделать ставку (bet)
            |2. Посмотреть баланс (balance)
            |3. Новый игрок (newPlayer)
            |4. Войти (logIn)
            |""".stripMargin)
          response <- WebSocketBuilder[IO].build(receive = fromClient, send = toClient)
        } yield response
    }
  }

//private def userIdentifierRoute(topic: Topic[IO, String], ref: Ref[IO, Map[String, Player]], refLogin: Ref[IO, Login]) =
//      HttpRoutes.of[IO] {
//
//        //curl -X POST "localhost:9001/player" -H "Content-Type: application/json" -d '{"login": masana, "password": 123fdsf1315}'
//        case req @ POST -> Root / "player" =>
//
//          def verifyPlayer(player: Player): IO[String] = {
//            for {
//              playerReal <- ref.modify { playerRef =>
//                val verify = playerRef.get(player.login.login)
//                verify match {
//                  case None => for{
//                    _ <- println("No existing player. Create?")
//                    _ <- WebSocketBuilder[IO].build(
//                      receive = topic.publish
//                        .compose[Stream[IO, WebSocketFrame]](_.collect {
//                          case WebSocketFrame.Text(message, _) => message.trim match {
//                            case "1" => createPlayer(player)
//                            case "2" => balance()
//                          }
//                    )
//                    playerRef + (player.login.login -> player)
//                  }, s"Пользователь создан")
//                  case Some(playerExist) if (playerExist.password != player.password) => (playerRef, "Wrong password")
//                  case Some(playerExist) => (playerRef, s"Добро пожаловать, ${playerExist.login.login}")
//                }
//              }
//              _ <- refLogin.modify()
//              _ <- topic.publish1(playerReal)
//            } yield playerReal
//          }
//
//          req.as[Player].flatMap { player =>
//              Ok(verifyPlayer(player))
//          }.handleErrorWith(e => BadRequest(s"Ха, лох, ${e.getMessage}"))
//      }


  private[server] def httpApp(topic: Topic[IO, String], ref: Ref[IO, Map[String, Player]], refLogin: Ref[IO, Login]) = {
    messageRoute(topic, ref) <+> connectionRoute
//    <+> userIdentifierRoute(topic, ref, login)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      ref <- Ref[IO].of(Map.empty[String, Player])
      refLogin <- Ref[IO].of(Login(""))
      topic <- Topic[IO, String]("Welcome. Write your request")
//      httpAp <- httpApp(topic, ref)
      _ <- BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withHttpApp(httpApp(topic, ref, refLogin))
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