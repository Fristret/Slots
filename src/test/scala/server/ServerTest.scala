package server

import cats.effect.IO
import cats.implicits.catsSyntaxOptionId
import fs2.Stream
import org.http4s.headers.`Content-Type`
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.{EmptyBody, EntityBody, Headers, HttpApp, MediaType, Method, Request, Response, Status, Uri}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import server.Server._
import org.http4s.server.websocket.websocketKey
import org.http4s.websocket.{WebSocketContext, WebSocketFrame}
import org.scalatest.OptionValues
import org.http4s.circe.CirceEntityCodec._
import server.MessageJson._
import server.CommonClasses.Token


class ServerClass extends AnyFreeSpec with Matchers with OptionValues{

  "Server should" - {
    "accept valid transaction request" in {
      val response = httpApp.flatMap(
        _.run(
          makeAuthorizationRequest(
            //            body = "{\"mail\":{\"value\":\"d\"}, \"player\":{\"login\": {\"value\": \"masana23\"},\"password\": {\"value\": \"mig943g\"}}}",
            body = s"""{"login": {"value": "masana23"}, "password": {"value": "mig943g"}}""",
          )
        )
      )
      val response1 = httpApp.flatMap(
        _.run(
          makeAuthorizationRequest(
            //            body = "{\"mail\":{\"value\":\"d\"}, \"player\":{\"login\": {\"value\": \"masana23\"},\"password\": {\"value\": \"mig943g\"}}}",
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

    "get balance" in {

      val message = """{"message":"balance"}"""
      val response = httpApp.flatMap(
        _.run(
          makeAuthorizationRequest(
            body = s"""{"login": {"value": "abobik23"}, "password": {"value": "1241244"}}""",
          )
        )
      )
      val token = response.unsafeRunSync().as[Token].unsafeRunSync()
      val uri = uri"/message" / token.id

            for {
              httpApp <- httpApp

              webSocketContext <- runMessageRequest(httpApp, uri)

            }yield ()
    }

    //    "do bet" in {
    //      val transactions: List[Int] =
    //        List(
    //          100,
    //          200,
    //          100,
    //          10,
    //          10,
    //          1000,
    //          20,
    //          30,
    //          20,
    //        )
    //      val request = httpApp.flatMap(
    //        _.run(
    //          makeAuthorizationRequest(
    //            body = s"""{"login": {"value": "abobik23"}, "password": {"value": "1241244"}}""",
    //          )
    //        )
    //      )
    //    }
    //  }


    //  "should stream balances via WebSocket as expected" in {
    //    val transactions: List[(String, BigDecimal)] =
    //      List(
    //        (playerId1, 100),
    //        (playerId1, -12.34),
    //        (playerId2, 999.999),
    //        (playerId1, 71.42),
    //        (playerId2, 33.41),
    //        (playerId2, 12.50),
    //        (playerId1, 0.01),
    //        (playerId2, 333),
    //        (playerId1, 5),
    //      )
    //    val requests: List[Request[IO]] = transactions.map {
    //      case (playerId, amount) =>
    //        makeTransactionRequest(
    //          body = s"""{"playerId": "$playerId", "deltaAmount": $amount}""",
    //        )
    //    }
    //    val balances: List[(String, BigDecimal)] = {
    //      val currentBalances = mutable.Map.empty[String, BigDecimal]
    //      transactions.map {
    //        case (playerId, amount) =>
    //          val newBalance = currentBalances.getOrElse(playerId, BigDecimal(0)) + amount
    //          currentBalances.update(playerId, newBalance)
    //          (playerId, newBalance)
    //      }
    //    }
    //
    //    (for {
    //      httpApp <- httpApp
    //
    //      // Open two WebSocket connections
    //      webSocketContext1 <- runBalanceRequest(httpApp)
    //      webSocketContext2 <- runBalanceRequest(httpApp)
    //
    //      // Execute HTTP POST requests one by one and verify WebSocket connections
    //      _ <- requests.mapWithIndex { (request, index) =>
    //        for {
    //          response <- httpApp.run(request)
    //          _ <- IO(response.status shouldBe Status.Accepted)
    //
    //          (expectedPlayerId, expectedBalance) = balances.get(index).value
    //          _ <- verifyBalance(webSocketContext1, expectedPlayerId, expectedBalance)
    //          _ <- verifyBalance(webSocketContext2, expectedPlayerId, expectedBalance)
    //        } yield ()
    //      }.sequence
    //    } yield ()).unsafeRunSync()
    //  }
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

  private def runMessageRequest(httpApp: HttpApp[IO], uri: Uri): IO[WebSocketContext[IO]] =
    httpApp.run(
      makeRequest(
        method = Method.GET,
        headers = Headers.empty,
        uri = uri,
        body = None,
      )
    ).map(
      _.attributes.lookup(websocketKey[IO]).value
    )

  private def verifyResponseStatus[A](
                                       response: IO[Response[IO]],
                                       expectedStatus: Status,
                                     ): Unit = (for {
    response <- response
    _ <- IO(response.status shouldBe expectedStatus)
  } yield ()).unsafeRunSync()
}

object ServerTest {
  val login = "masana"
  val login2 = "masana2344"
}