package server

import cats.effect.IO
import io.circe.syntax.EncoderOps
import org.http4s.headers.`Content-Type`
import org.http4s.{Headers, MediaType, Method, Request, Response, Status}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import server.Server._
import server.models.Protocol.{Login, Mail, NewPlayer, Password, Player}
import ForServerTest._
import io.circe.Json
import server.json.MessageJson._
import server.service.Doobie.deletePlayer
import org.http4s.circe.CirceEntityCodec._
import org.http4s.implicits.http4sLiteralsSyntax


class ServerTest extends AnyFreeSpec with Matchers{

  "Server should" - {
    "reject invalid authorization" in {
      val response = httpApp.flatMap(_.run(
        makeAuthorizationRequest(Json.fromString("invalid"))
      ))
      verifyResponseStatus(response, Status.BadRequest)
    }

    "create and login player" in {
      lazy val response = httpApp.flatMap(_.run(makeAuthorizationRequest(newPlayerTest.asJson))
      )
      verifyResponseStatus(response, Status.Ok)
      lazy val response1 = httpApp.flatMap(
        _.run(
          makeAuthorizationRequest(
            playerTest.asJson
          )
        )
      )
      verifyResponseStatus(response1, Status.Ok)
      deletePlayer(loginTest).unsafeRunSync()
    }
  }

  private def makeAuthorizationRequest(body: Json): Request[IO] = Request(Method.POST, uri = uri"/authorization", headers = Headers.of(`Content-Type`(MediaType.application.json))).withEntity(body)

  private def verifyResponseStatus(
                                       response: IO[Response[IO]],
                                       expectedStatus: Status,
                                     ): Unit = (for {
    response <- response
    _ <- IO(response.status shouldBe expectedStatus)
  } yield ()).unsafeRunSync()
}

object ForServerTest {
  val loginTest: Login = Login("testLogin")
  val passwordTest: Password = Password("testPassword")
  val mailTest: Mail = Mail("test@mail.ru")
  val playerTest: Player = Player(loginTest, passwordTest)
  val newPlayerTest: NewPlayer = NewPlayer(mailTest, playerTest)
}
