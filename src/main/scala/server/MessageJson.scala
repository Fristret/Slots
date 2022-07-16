package server

import io.circe.{Decoder, Encoder}
import server.Protocol._

object MessageJson {
  import io.circe.generic.semiauto._
  implicit val betDecoder: Decoder[Bet] = deriveDecoder[Bet]
  implicit val betEncoder: Encoder[Bet] = deriveEncoder[Bet]

  implicit val messageDecoder: Decoder[Message] = deriveDecoder
  implicit val messageEncoder: Encoder[Message] = deriveEncoder

  implicit val playerDecoder: Decoder[Player] = deriveDecoder[Player]
  implicit val playerEncoder: Encoder[Player] = deriveEncoder[Player]
}
