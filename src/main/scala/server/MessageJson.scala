package server

import cats.implicits.toFunctorOps
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import server.Protocol._
import server.CommonClasses._

object MessageJson {
  import io.circe.generic.semiauto._
  implicit val betDecoder: Decoder[Bet] = deriveDecoder[Bet]
  implicit val betEncoder: Encoder[Bet] = deriveEncoder[Bet]

  implicit val balanceDecoder: Decoder[Balance] = deriveDecoder[Balance]
  implicit val balanceEncoder: Encoder[Balance] = deriveEncoder[Balance]

  implicit val messageInDecoder: Decoder[MessageIn] = List[Decoder[MessageIn]](betDecoder.widen, balanceDecoder.widen).reduce(_ or _)
  implicit val messageInEncoder: Encoder[MessageIn] = Encoder.instance{
    case bet: Bet => bet.asJson
    case bal: Balance => bal.asJson
  }

  implicit val tokenDecoder: Decoder[Token] = deriveDecoder[Token]
  implicit val tokenEncoder: Encoder[Token] = deriveEncoder[Token]

  implicit val playerDecoder: Decoder[Player] = deriveDecoder[Player]
  implicit val playerEncoder: Encoder[Player] = deriveEncoder[Player]
}
