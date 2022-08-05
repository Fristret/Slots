package server

import cats.effect._
import cats.effect.concurrent.Ref
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import server.Cache._
import server.CommonClasses.Token

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.ExecutionContext

class CacheTest extends AnyFreeSpec with Matchers{

  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  "Cache should" - {
    "should be empty" in {
      val cacheEmpty = Ref[IO].of(Map.empty[Token, Instant])
      for {
        cache <- cacheEmpty
        map <- cache.get
      } yield check(map) should be (Map.empty[Token, Instant])
    }

    "clean cache" in {
      val token1 = Token("f230557b-f0bb-4c5f-91c4-b5acaeb53da9&masana23")
      val token2 = Token("f230557b-f0bb-4c5f-91c3-b5acaeb53da9&masana2323")
      val cacheNonEmpty = Ref[IO].of(Map(token1 -> Instant.now().minus(2.toLong, ChronoUnit.MINUTES), token2 -> Instant.now()))
      for {
        cache <- cacheNonEmpty
        map <- cache.get
      } yield check(map) should not be map
    }
    "clean all cache" in {
      val token1 = Token("f230557b-f0bb-4c5f-91c4-b5acaeb53da9&masana23")
      val token2 = Token("f230557b-f0bb-4c5f-91c3-b5acaeb53da9&masana2323")
      val time = Instant.now().minus(2.toLong, ChronoUnit.MINUTES)
      val cacheNonEmpty = Ref[IO].of(Map(token1 -> time, token2 -> time))
      for {
        cache <- cacheNonEmpty
        map <- cache.get
      } yield check(map) should be (Map.empty[Token, Instant])
    }
  }

}
