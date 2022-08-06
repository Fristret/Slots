package server.service

import cats.Monad
import cats.effect.Timer
import cats.effect.concurrent.Ref
import cats.implicits._
import server.models.CommonClasses.Token

import java.time.Instant
import scala.concurrent.duration.DurationInt

trait Cache[F[_]] {
  def check(map: Map[Token, Instant]): Map[Token, Instant]
  def cacheOptimizer(implicit ev: Timer[F]): F[Unit]
}

object Cache {

  def apply[F[_] : Monad](cache: Ref[F, Map[Token, Instant]]): Cache[F] = new Cache[F] {
    def check(map: Map[Token, Instant]): Map[Token, Instant] =
      map
        .filterNot { tuple => Instant.now().compareTo(tuple._2) >= 0}

    def cacheOptimizer(implicit ev: Timer[F]): F[Unit] = for {
      map <- cache.get
      modified = check(map)
      _ <- cache.set(modified)
      _ <- ev.sleep(30.seconds)
      _ <- cacheOptimizer
    } yield ()

  }
}
