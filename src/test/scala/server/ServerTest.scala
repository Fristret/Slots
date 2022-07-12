package server

import cats.effect.IO
import cats.syntax.all._
import server.SlotServer.httpApp
import server.ServerTest._
import fs2.Stream
import io.circe.parser._
import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s.server.websocket.websocketKey
import org.http4s.websocket.WebSocketContext
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import scala.collection.mutable
object ServerTest{

  private def makeTransactionRequest(body: String): Request[IO] =
    makeRequest(
      method = Method.POST,
      headers = Headers.of(`Content-Type`(MediaType.application.json)),
      uri = uri"/user",
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
}