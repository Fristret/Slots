package server

import cats.effect._
import cats.effect.concurrent.Ref
import server.CommonClasses.Token

import java.time.Instant

import scala.concurrent.duration.DurationInt

object Cache {

  def check(cache: Ref[IO, Map[Token, Instant]]): IO[Map[Token, Instant]] = cache.modify {
    map =>
      (map.foldLeft(map) {
        case (value, (key, instant)) => if (Instant.now().compareTo(instant) >= 0) map.removed(key)
        else map
      }, map)
  }

  def cacheOptimizer(cache: Ref[IO, Map[Token, Instant]])(implicit ev: Timer[IO]): IO[Unit] = for {
    _ <- cache.get.flatMap(map => IO(println(map)))
    _ <- check(cache)
    _ <- IO(println("Cache checked"))
    _ <- cache.get.flatMap(map => IO(println(map)))
    _ <- ev.sleep(1.minute)
    _ <- cacheOptimizer(cache)
  } yield ()

}
