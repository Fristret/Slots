package server

import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import fs2.concurrent._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits._

import scala.concurrent.ExecutionContext
import Routes._
import server.models.CommonClasses.Token
import game.models.RPGElements.Stage
import server.models.Protocol.Login
import server.service._

import java.time.Instant

object Server extends IOApp {

  private[server] val httpApp = for {
    cache <- Ref[IO].of(Map.empty[Token, Instant])
    rpgProgress <- Ref[IO].of(Map.empty[Login, Stage])
    _ <- Doobie.run
    cacheOpt = Cache(cache)
    _ <- cacheOpt.cacheOptimizer.start
    topic <- Topic[IO, String]("Global")
  } yield messageRoute(topic, cache, rpgProgress).orNotFound

  override def run(args: List[String]): IO[ExitCode] = for {
    httpApp <- httpApp
    _ <- BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
  }yield ExitCode.Success
}