package server

import cats.Monad
import cats.effect.{ExitCode, IO, IOApp, Resource, Timer}
import cats.implicits._
import io.circe.syntax.EncoderOps
import io.jvm.uuid.UUID
import org.http4s.Method.POST
import org.http4s.Request
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.implicits.http4sLiteralsSyntax
import server.models.CommonClasses._
import server.models.Protocol._

import java.net.http.HttpClient
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

object Client extends IOApp {

  import server.json.MessageJson._
  import org.http4s.circe.CirceEntityCodec._

  private def sendMessage[F[_]: Monad](connectionHighLevel: WSConnectionHighLevel[F], string: String, times: Int, count: Int = 1)(implicit ev: Timer[F]): F[Unit] = for {
    _ <- connectionHighLevel.send(WSFrame.Text(string))
    _ <- println().pure[F]
    _ <- ev.sleep(3000.milliseconds)
    _ <- if (count >= times) println("Success").pure[F]
      else sendMessage(connectionHighLevel, string, times, count + 1)
  } yield ()

  private val uri = uri"http://localhost:9001/authorization"

  private def clientResource(token: Token): Resource[IO, WSConnectionHighLevel[IO]] = Resource.eval(IO(HttpClient.newHttpClient()))
    .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri"ws://127.0.0.1:9001/game/" / token.id)))

  private val login1 = Login("masana23")
  private val password1 = Password("qwerty12345")
  private val mail = Mail("masana23@mail.ru")
  private val player = Player(login1, password1)
  private val newPlayer = NewPlayer(mail, player)

  private val jsonReg = newPlayer.asJson
  private val jsonAut = player.asJson

  override def run(args: List[String]): IO[ExitCode] = for {
    tok <- BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
      val registrationReq = Request[IO](method = POST, uri).withEntity(jsonReg)
      val logInReq = Request[IO](method = POST, uri).withEntity(jsonAut)
      for {
        message <- client.fetchAs[Message](registrationReq).handleErrorWith(e => IO(s"${e.getMessage}"))
        _ <- IO(println(message))
        _ <- Timer[IO].sleep(5000.milliseconds)
        token <- client.fetchAs[Token](logInReq).option.map {
          case Some(value) => value
          case None => Token("0")
        }
        _ <- IO(println(token))
        _ <- Timer[IO].sleep(5000.milliseconds)
      } yield token
    }
    _ <- clientResource(tok).use { conn => {
      for {
        _ <- conn.receiveStream.collect {
          case WSFrame.Text(s, _) => println(s)
        }.compile.drain.start
        _ <- sendMessage(conn, """{"amount": "200"}""", 2)
        _ <- sendMessage(conn, """invalid""", 1)
        _ <- sendMessage(conn, """{"amount": "20000000"}""", 1)
        _ <- sendMessage(conn, """{"spirit": "Fire"}""", 1)
        _ <- sendMessage(conn, """{"amount": "200"}""", 1)
        _ <- conn.sendClose("No reason")
      } yield ()
    }
    }.handleErrorWith(e => IO(s"${e.getMessage}"))
  } yield ExitCode.Success
}
