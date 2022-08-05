package server

import cats.effect.{ExitCode, IO, IOApp, Resource}
import io.circe.Json
import org.http4s.Method.POST
import org.http4s._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.implicits.http4sLiteralsSyntax
import Protocol._

import java.net.http.HttpClient
import scala.concurrent.ExecutionContext
import CommonClasses._
import cats.implicits.catsSyntaxFlatMapOps


object Client extends IOApp {

  import MessageJson._
  import org.http4s.circe.CirceEntityCodec._

  val listOfMessages: Seq[String] = List("""{"amount": "200"}""","""{"amount": "200"}""", """{"amount": "200"}""", """{"message": "balance"}""")

  private val uri = uri"http://localhost:9001/authorization"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  private def clientResource(token: Token): Resource[IO, WSConnectionHighLevel[IO]] = Resource.eval(IO(HttpClient.newHttpClient()))
    .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri"ws://127.0.0.1:9001/message/" / token.id)))

  override def run(args: List[String]): IO[ExitCode] = for {
      tok <- BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
        for {
          _ <- printLine("Im Born")
          json = io.circe.parser.parse(s"""{"mail": {"value": "masana23@mail.ru"}, "player": {"login": {"value": "abobik231414"}, "password": {"value": "1241244"}}}""").getOrElse(Json.Null)
          json2 = io.circe.parser.parse(s"""{"login": {"value": "abobik231414"}, "password": {"value": "1241244"}}""").getOrElse(Json.Null)
          registration = Request[IO](method = POST, uri).withEntity(json)
          string <- client.fetchAs[String](registration)
          _ <- printLine(string)
          logIn = Request[IO](method = POST, uri).withEntity(json2)
          token <- client.fetchAs[Token](logIn)
        } yield token
      }
      _ <- clientResource(tok).use { conn =>
            {for {
              _ <- conn.receiveStream.collectFirst {
                case WSFrame.Text(s, _) => s
              }.compile.string >>= printLine
              _ <- conn.send(WSFrame.Text("""{"message": "balance"}"""))
              _ <- conn.receiveStream.collectFirst {
                case WSFrame.Text(s, _) => s
              }.compile.string >>= printLine
                  _ <- conn.send(WSFrame.Text("""{"amount": "200"}"""))
                  _ <- conn.receiveStream.collectFirst {
                    case WSFrame.Text(s, _) => s
                  }.compile.string >>= printLine
                  _ <- conn.receiveStream.collectFirst {
                    case WSFrame.Text(s, _) => s
                  }.compile.string >>= printLine
                  _ <- conn.receiveStream.collectFirst {
                    case WSFrame.Text(s, _) => s
                  }.compile.string >>= printLine
                  _ <- conn.sendClose("No reason")
              } yield ()}.handleErrorWith(e => IO(s"${e.getMessage}"))
          }
    }yield ExitCode.Success
}