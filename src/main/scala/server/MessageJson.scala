package server

import cats.implicits._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import server.Protocol._
import server.CommonClasses._
import Game.RPGElements._
import Game.SlotObjects._


object MessageJson {
  import io.circe.generic.semiauto._
  implicit val betDecoder: Decoder[Bet] = deriveDecoder[Bet]
  implicit val betEncoder: Encoder[Bet] = deriveEncoder[Bet]

  implicit val loginDecoder: Decoder[Login] = deriveDecoder[Login]
  implicit val loginEncoder: Encoder[Login] = deriveEncoder[Login]

  implicit val passwordDecoder: Decoder[Password] = deriveDecoder[Password]
  implicit val passwordEncoder: Encoder[Password] = deriveEncoder[Password]

  implicit val mailDecoder: Decoder[Mail] = deriveDecoder[Mail]
  implicit val mailEncoder: Encoder[Mail] = deriveEncoder[Mail]

  implicit val tokenDecoder: Decoder[Token] = deriveDecoder[Token]
  implicit val tokenEncoder: Encoder[Token] = deriveEncoder[Token]

  implicit val playerDecoder: Decoder[Player] = deriveDecoder[Player]
  implicit val playerEncoder: Encoder[Player] = deriveEncoder[Player]

  implicit val newPlayerDecoder: Decoder[NewPlayer] = deriveDecoder[NewPlayer]
  implicit val newPlayerEncoder: Encoder[NewPlayer] = deriveEncoder[NewPlayer]

  implicit val balanceDecoder: Decoder[Balance] = deriveDecoder[Balance]
  implicit val balanceEncoder: Encoder[Balance] = deriveEncoder[Balance]

  implicit val winOutputDecoder: Decoder[WinOutput] = deriveDecoder
  implicit val winOutputEncoder: Encoder[WinOutput] = deriveEncoder

  implicit val messageInDecoder: Decoder[MessageIn] = List[Decoder[MessageIn]](betDecoder.widen, balanceDecoder.widen, playerDecoder.widen, newPlayerDecoder.widen).reduce(_ or _)
  implicit val messageInEncoder: Encoder[MessageIn] = Encoder.instance{
    case bet: Bet => bet.asJson
    case bal: Balance => bal.asJson
    case player: Player => player.asJson
    case newPlayer: NewPlayer => newPlayer.asJson
  }
}
