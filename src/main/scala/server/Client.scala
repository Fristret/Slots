package server

import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource}
import cats.implicits.catsSyntaxFlatMapOps
import org.http4s.Request
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.implicits.http4sLiteralsSyntax

import java.net.http.HttpClient
import scala.concurrent.ExecutionContext

object Client extends IOApp {

  private val uri = uri"ws://127.0.0.1:9001/message"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  override def run(args: List[String]): IO[ExitCode] = {
    val clientResource: Resource[IO, WSConnectionHighLevel[IO]] = Resource.eval(IO(HttpClient.newHttpClient(


    )))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))


    clientResource.use { client =>
      for {
        login <- IO(scala.io.StdIn.readLine())
        password <- IO(scala.io.StdIn.readLine())
        _ <- client.send(WSFrame.Text(s"""{"login": $login, "password": $password}"""))
        _ <- client.receiveStream.collectFirst {
          case WSFrame.Text(s, _) => s
        }.compile.string >>= printLine
      } yield ExitCode.Success
    }
  }
}
