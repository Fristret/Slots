package server

import cats.{Applicative, Monad}
import cats.effect.{ExitCode, IO, IOApp, Resource, Timer}
import cats.implicits._
import io.circe.Json
import org.http4s.Method.POST
import org.http4s.Request
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.implicits.http4sLiteralsSyntax
import server.models.CommonClasses._

import java.net.http.HttpClient
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

object Client extends IOApp {

  import server.json.MessageJson._
  import org.http4s.circe.CirceEntityCodec._

  private def sendMessage[F[_]: Monad : Applicative](connectionHighLevel: WSConnectionHighLevel[F], string: String, times: Int, count: Int = 1)(implicit ev: Timer[F]): F[Unit] = for {
    _ <- connectionHighLevel.send(WSFrame.Text(string))
    _ <- ev.sleep(1000.milliseconds)
    _ <- if (count >= times) println("Success").pure[F]
      else sendMessage(connectionHighLevel, string, times, count + 1)
  } yield ()

  private val uri = uri"http://localhost:9001/authorization"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  private def clientResource(token: Token): Resource[IO, WSConnectionHighLevel[IO]] = Resource.eval(IO(HttpClient.newHttpClient()))
    .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri"ws://127.0.0.1:9001/message/" / token.id)))

  private val jsonAut = io.circe.parser.parse(s"""{"login": {"value": "abobik23141423"}, "password": {"value": "1241244"}}""").getOrElse(Json.Null)
  private val jsonReg = io.circe.parser.parse(s"""{"mail": {"value": "masana23@mail.ru"}, "player": {"login": {"value": "abobik23141423"}, "password": {"value": "1241244"}}}""").getOrElse(Json.Null)

  override def run(args: List[String]): IO[ExitCode] = for {
    tok <- BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
      for {
        _ <- printLine("Im Born")
        registration = Request[IO](method = POST, uri).withEntity(jsonReg)
        error <- client.fetchAs[ErrorMessage](registration)
        _ <- IO(println(error))
        logIn = Request[IO](method = POST, uri).withEntity(jsonAut)
        token <- client.fetchAs[Token](logIn)
      } yield token
    }
    _ <- clientResource(tok).use { conn => {
      for {
        _ <- conn.receiveStream.collect {
          case WSFrame.Text(s, _) => println(s)
        }.compile.drain.start
        _ <- sendMessage(conn, """{"amount": "200"}""", 20)
        _ <- sendMessage(conn, """{"message": "balance"}""", 1)
        _ <- conn.sendClose("No reason")
      } yield ()
    }.handleErrorWith(e => IO(s"${e.getMessage}"))
    }
  } yield ExitCode.Success
}
