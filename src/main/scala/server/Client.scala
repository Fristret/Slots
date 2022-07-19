package server

import cats.effect.{ExitCode, IO, IOApp, Resource}
import Protocol._
import io.circe.syntax.EncoderOps
import org.http4s.Method.POST
import org.http4s._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSRequest}
import org.http4s.implicits.http4sLiteralsSyntax

import java.net.http.HttpClient
import scala.concurrent.ExecutionContext
import scala.io.StdIn

object Client extends IOApp {

  import MessageJson._
  import org.http4s.circe.CirceEntityCodec._

  private val uri = uri"http://localhost:9001/registration"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  val clientResource: Resource[IO, WSConnectionHighLevel[IO]] = Resource.eval(IO(HttpClient.newHttpClient()))
    .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
      for {
      _ <- printLine("Im Born. Enter your message: 1. NewPlayer, 2. LogIn")
        message = StdIn.readLine().trim match {
          case "1" => "NewPlayer"
          case "2" => "LogIn"
        }
        _ <- printLine(message)
        _ <- printLine("Enter your login")
        login = StdIn.readLine().trim
        _ <- printLine("Enter your password")
        password = StdIn.readLine().trim
        json = s"""{"message": $message, "login": $login, "password": $password}""".asJson
        request = Request[IO](method = POST, uri).withEntity(json)
        _ <- printLine(request.toString)
        _ <- client.expect[Player](request)
      } yield ExitCode.Success
    }
  }
}
