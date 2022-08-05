package server

import cats.effect._
import cats.effect.concurrent.Ref
import server.CommonClasses.Token

import java.time.Instant

import scala.concurrent.duration.DurationInt

object Cache {

  def check(map: Map[Token, Instant]): Map[Token, Instant] =
    map
      .filterNot { tuple => Instant.now().compareTo(tuple._2) >= 0}

  def cacheOptimizer(cache: Ref[IO, Map[Token, Instant]])(implicit ev: Timer[IO]): IO[Unit] = for {
    map <- cache.get
    modified = check(map)
//    _ <- IO(println(modified))
//    _ <- IO(println("Cache checked"))
    _ <- cache.set(modified)
    _ <- ev.sleep(30.seconds)
    _ <- cacheOptimizer(cache)
  } yield ()

}
