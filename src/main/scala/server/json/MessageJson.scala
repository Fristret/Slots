package server.json

import cats.implicits.toFunctorOps
import game.models.SlotObjects._
import game.models.RPGElements._
import io.circe.generic.extras.semiauto.deriveEnumerationCodec
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, Encoder}
import server.models.CommonClasses._
import server.models.Protocol._

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

  implicit val actionRPGCodec: Decoder[ActionRPG] = deriveEnumerationCodec[ActionRPG]

  implicit val ammunitionDecoder: Decoder[Ammunition] = deriveDecoder
  implicit val ammunitionEncoder: Encoder[Ammunition] = deriveEncoder

  implicit val heroDecoder: Decoder[Hero] = deriveDecoder
  implicit val heroEncoder: Encoder[Hero] = deriveEncoder

  implicit val enemyTypeCodec: Codec[EnemyType] = deriveEnumerationCodec[EnemyType]

  implicit val enemyDecoder: Decoder[Enemy] = deriveDecoder
  implicit val enemyEncoder: Encoder[Enemy] = deriveEncoder

  implicit val stageDecoder: Decoder[Stage] = deriveDecoder
  implicit val stageEncoder: Encoder[Stage] = deriveEncoder

  implicit val elementCodec: Codec[Element] = deriveEnumerationCodec[Element]

  implicit val errorMessageDecoder: Decoder[ErrorMessage] = deriveDecoder
  implicit val errorMessageEncoder: Encoder[ErrorMessage] = deriveEncoder

  implicit val configureDecoder: Decoder[Configure] = deriveDecoder
  implicit val configureEncoder: Encoder[Configure] = deriveEncoder

  implicit val winDecoder: Decoder[Win] = deriveDecoder
  implicit val winEncoder: Encoder[Win] = deriveEncoder

  implicit val playerDecoder: Decoder[Player] = deriveDecoder[Player]
  implicit val playerEncoder: Encoder[Player] = deriveEncoder[Player]

  implicit val newPlayerDecoder: Decoder[NewPlayer] = deriveDecoder[NewPlayer]
  implicit val newPlayerEncoder: Encoder[NewPlayer] = deriveEncoder[NewPlayer]

  implicit val balanceDecoder: Decoder[Balance] = deriveDecoder[Balance]
  implicit val balanceEncoder: Encoder[Balance] = deriveEncoder[Balance]

  implicit val balanceOutDecoder: Decoder[BalanceOut] = deriveDecoder
  implicit val balanceOutEncoder: Encoder[BalanceOut] = deriveEncoder

  implicit val winOutputDecoder: Decoder[WinOutput] = deriveDecoder
  implicit val winOutputEncoder: Encoder[WinOutput] = deriveEncoder

  implicit val messageInDecoder: Decoder[MessageIn] = List[Decoder[MessageIn]](betDecoder.widen, balanceDecoder.widen, playerDecoder.widen, newPlayerDecoder.widen).reduce(_ or _)
  implicit val messageInEncoder: Encoder[MessageIn] = Encoder.instance {
    case bet: Bet => bet.asJson
    case bal: Balance => bal.asJson
    case player: Player => player.asJson
    case newPlayer: NewPlayer => newPlayer.asJson
  }
}
