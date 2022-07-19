package server

import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.toSemigroupKOps
import fs2.concurrent._
import org.http4s.server.blaze.BlazeServerBuilder

import org.http4s.implicits._

import scala.concurrent.ExecutionContext
import Protocol._
import Routes._
// Complete all TODOs below according to their description.

object Server extends IOApp {

  private[server] def httpApp(topic: Topic[IO, String], ref: Ref[IO, Map[String, Player]]) = {
    messageRoute(topic, ref) <+> registrationRoute(ref)
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