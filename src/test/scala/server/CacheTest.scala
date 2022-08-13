package server

import cats.effect._
import cats.effect.concurrent.Ref
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import server.models.CommonClasses.Token
import server.service.Cache

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.ExecutionContext

class CacheTest extends AsyncFreeSpec with AsyncIOSpec with Matchers{

  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)


  "Cache should" - {
    "should be empty" in {
      val cacheEmpty = Ref[IO].of(Map.empty[Token, Instant])
      val effect = for {
        cache <- cacheEmpty
        cacheTest = Cache(cache)
        map <- cache.get
      } yield cacheTest.check(map)
      effect.asserting(_ shouldBe Map.empty[Token, Instant])
    }

    "clean cache" in {
      val token1 = Token("f230557b-f0bb-4c5f-91c4-b5acaeb53da9&masana23")
      val token2 = Token("f230557b-f0bb-4c5f-91c3-b5acaeb53da9&masana2323")
      val time1 = Instant.now().plus(2.toLong, ChronoUnit.MINUTES)
      val time2 = Instant.now()
      val cacheNonEmpty = Ref[IO].of(Map(token1 -> time1, token2 -> time2))
      val effect = for {
        cache <- cacheNonEmpty
        cacheTest = Cache(cache)
        map <- cache.get
        newMap = cacheTest.check(map)
      } yield newMap
      effect.asserting(_ shouldBe Map(token1 -> time1))
    }

    "clean all cache" in {
      val token1 = Token("f230557b-f0bb-4c5f-91c4-b5acaeb53da9&masana23")
      val token2 = Token("f230557b-f0bb-4c5f-91c3-b5acaeb53da9&masana2323")
      val time = Instant.now().minus(2.toLong, ChronoUnit.MINUTES)
      val cacheNonEmpty = Ref[IO].of(Map(token1 -> time, token2 -> time))
      val effect = for {
        cache <- cacheNonEmpty
        cacheTest = Cache(cache)
        map <- cache.get
      } yield cacheTest.check(map)
      effect.asserting(_ shouldBe Map.empty[Token, Instant])
    }
  }

}
