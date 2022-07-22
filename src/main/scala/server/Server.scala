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
import server.CommonClasses.Token
import Cache._

import java.time.Instant

object Server extends IOApp {

  private[server] def httpApp(topic: Topic[IO, String], ref: Ref[IO, Map[String, Player]], cache: Ref[IO, Map[Token, Instant]]) = {
    messageRoute(topic, cache) <+> registrationRoute(ref, cache)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      cache <- Ref[IO].of(Map.empty[Token, Instant])
      _ <- cacheOptimizer(cache).start
      ref <- Ref[IO].of(Map.empty[String, Player])
      topic <- Topic[IO, String]("Welcome. Write your request")
      _ <- BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withHttpApp(httpApp(topic, ref, cache))
        .serve
        .compile
        .drain
    } yield ExitCode.Success
  }
}