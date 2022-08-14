package server.json

import cats.implicits.toFunctorOps
import game.models.MiniGameObjects.{MiniGameOutput, MiniGameUnit}
import game.models.SlotObjects._
import game.models.RPGElements._
import io.circe.generic.extras.semiauto.deriveEnumerationCodec
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, Encoder}
import server.models.CommonClasses._
import server.models.Protocol._

object MessageJson {

  import io.circe.generic.semiauto._

  implicit val miniGameOptionDecoder: Decoder[MiniGameOption] = deriveDecoder
  implicit val miniGameOptionEncoder: Encoder[MiniGameOption] = deriveEncoder

  implicit val rpgUpdateStageDecoder: Decoder[RPGUpdateStage] = deriveDecoder
  implicit val rpgUpdateStageEncoder: Encoder[RPGUpdateStage] = deriveEncoder

  implicit val actionOutputDecoder: Decoder[ActionOutput] = deriveDecoder
  implicit val actionOutputEncoder: Encoder[ActionOutput] = deriveEncoder

  implicit val payLineDecoder: Decoder[PayLine] = deriveDecoder
  implicit val payLineEncoder: Encoder[PayLine] = deriveEncoder

  implicit val betDecoder: Decoder[Bet] = deriveDecoder
  implicit val betEncoder: Encoder[Bet] = deriveEncoder

  implicit val loginDecoder: Decoder[Login] = deriveDecoder
  implicit val loginEncoder: Encoder[Login] = deriveEncoder

  implicit val passwordDecoder: Decoder[Password] = deriveDecoder
  implicit val passwordEncoder: Encoder[Password] = deriveEncoder

  implicit val mailDecoder: Decoder[Mail] = deriveDecoder
  implicit val mailEncoder: Encoder[Mail] = deriveEncoder

  implicit val tokenDecoder: Decoder[Token] = deriveDecoder
  implicit val tokenEncoder: Encoder[Token] = deriveEncoder

  implicit val actionRPGCodec: Codec[ActionRPG] = deriveEnumerationCodec

  implicit val ammunitionDecoder: Decoder[Ammunition] = deriveDecoder
  implicit val ammunitionEncoder: Encoder[Ammunition] = deriveEncoder

  implicit val heroDecoder: Decoder[Hero] = deriveDecoder
  implicit val heroEncoder: Encoder[Hero] = deriveEncoder

  implicit val enemyTypeCodec: Codec[EnemyType] = deriveEnumerationCodec

  implicit val enemyDecoder: Decoder[Enemy] = deriveDecoder
  implicit val enemyEncoder: Encoder[Enemy] = deriveEncoder

  implicit val stageDecoder: Decoder[Stage] = deriveDecoder
  implicit val stageEncoder: Encoder[Stage] = deriveEncoder

  implicit val elementCodec: Codec[Symbol] = deriveEnumerationCodec

  implicit val miniGameUnitCodec: Codec[MiniGameUnit] = deriveEnumerationCodec

  implicit val errorMessageDecoder: Decoder[ErrorMessage] = deriveDecoder
  implicit val errorMessageEncoder: Encoder[ErrorMessage] = deriveEncoder

  implicit val miniGameOutputDecoder: Decoder[MiniGameOutput] = deriveDecoder
  implicit val miniGameOutputEncoder: Encoder[MiniGameOutput] = deriveEncoder

  implicit val columnDecoder: Decoder[Column] = deriveDecoder
  implicit val columnEncoder: Encoder[Column] = deriveEncoder

  implicit val screenDecoder: Decoder[Reel] = deriveDecoder
  implicit val screenEncoder: Encoder[Reel] = deriveEncoder

  implicit val winDecoder: Decoder[Win] = deriveDecoder
  implicit val winEncoder: Encoder[Win] = deriveEncoder

  implicit val playerDecoder: Decoder[Player] = deriveDecoder
  implicit val playerEncoder: Encoder[Player] = deriveEncoder

  implicit val newPlayerDecoder: Decoder[NewPlayer] = deriveDecoder
  implicit val newPlayerEncoder: Encoder[NewPlayer] = deriveEncoder

  implicit val balanceOutputDecoder: Decoder[BalanceOutput] = deriveDecoder
  implicit val balanceOutputEncoder: Encoder[BalanceOutput] = deriveEncoder

  implicit val winOutputDecoder: Decoder[WinOutput] = deriveDecoder
  implicit val winOutputEncoder: Encoder[WinOutput] = deriveEncoder

  implicit val messageDecoder: Decoder[Message] = deriveDecoder
  implicit val messageEncoder: Encoder[Message] = deriveEncoder

  implicit val messageInDecoder: Decoder[MessageIn] = List[Decoder[MessageIn]](betDecoder.widen, playerDecoder.widen, newPlayerDecoder.widen, miniGameOptionDecoder.widen).reduce(_ or _)
  implicit val messageInEncoder: Encoder[MessageIn] = Encoder.instance {
    case bet: Bet => bet.asJson
    case player: Player => player.asJson
    case newPlayer: NewPlayer => newPlayer.asJson
    case miniGame: MiniGameOption => miniGame.asJson
  }
}
