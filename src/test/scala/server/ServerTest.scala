package server

import cats.effect.IO
import cats.implicits.catsSyntaxOptionId
import fs2.Stream
import org.http4s.headers.`Content-Type`
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.{EmptyBody, EntityBody, Headers, MediaType, Method, Request, Response, Status, Uri}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import server.Server._
import org.scalatest.OptionValues


class ServerTest extends AnyFreeSpec with Matchers with OptionValues{

  "Server should" - {
    "accept valid transaction request" in {
      val response = httpApp.flatMap(
        _.run(
          makeAuthorizationRequest(
            body = s"""{"login": {"value": "masana23"}, "password": {"value": "mig943g"}}""",
          )
        )
      )
      val response1 = httpApp.flatMap(
        _.run(
          makeAuthorizationRequest(
            body = s"""{"mail": {"value": "masana23@mail.ru"}, "player": {"login": {"value": "masana23"}, "password": {"value": "mig943g"}}}""",
          )
        )
      )
      verifyResponseStatus(response, Status.Ok)
      verifyResponseStatus(response1, Status.Ok)
    }

    "reject invalid transaction requests via HTTP POST" in {
      val response = httpApp.flatMap(
        _.run(
          makeAuthorizationRequest(
            body = "invalid",
          )
        )
      )
      verifyResponseStatus(response, Status.BadRequest)
    }

    "create and login player" in {
      val response = httpApp.flatMap(
        _.run(
          makeAuthorizationRequest(
            body = s"""{"login": {"value": "abobik23"}, "password": {"value": "1241244"}}""",
          )
        )
      )
      val response1 = httpApp.flatMap(
        _.run(
          makeAuthorizationRequest(
            body = s"""{"mail": {"value": "masana23@mail.ru"}, "player": {"login": {"value": "abobik23"}, "password": {"value": "1241244"}}}""",
          )
        )
      )
      verifyResponseStatus(response1, Status.Ok)
      verifyResponseStatus(response, Status.BadRequest)
    }
  }

  private def makeAuthorizationRequest(body: String): Request[IO] =
    makeRequest(
      method = Method.POST,
      headers = Headers.of(`Content-Type`(MediaType.application.json)),
      uri = uri"/authorization",
      body = body.some,
    )

  private def makeRequest(
                           method: Method,
                           headers: Headers,
                           uri: Uri,
                           body: Option[String],
                         ): Request[IO] =
    Request(
      method = method,
      uri = uri,
      headers = headers,
      body = body.fold[EntityBody[IO]](EmptyBody) { body =>
        Stream.emits(os = body.map(_.toByte))
      },
    )

  private def verifyResponseStatus[A](
                                       response: IO[Response[IO]],
                                       expectedStatus: Status,
                                     ): Unit = (for {
    response <- response
    _ <- IO(response.status shouldBe expectedStatus)
  } yield ()).unsafeRunSync()
}