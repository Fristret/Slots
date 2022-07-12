package server

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxFlatMapOps
import org.http4s.Request
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits.http4sLiteralsSyntax

import scala.concurrent.ExecutionContext

object Client extends IOApp {


  private val url = uri"http://localhost:9001"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO]).use { case (client, blocker) =>
      for {
        _ <- printLine("Greetings, Im server")
        _ <- client.expect[String](url / "connection") >>= printLine
        _ <- printLine()

      } yield ExitCode.Success
    }
}
