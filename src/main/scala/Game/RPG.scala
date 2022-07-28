package Game

import io.circe.Json
import io.circe.parser._
import io.circe.syntax.EncoderOps
import server.Protocol._
import server.MessageJson._

object RPG extends App{
  val player = s"""{"login": "masana", "password": "masanasd"}"""
  val newPlayer = s"""{"mail": "fnsdfsf", "player": $player}"""
  val pla =  Player(Login("masana"), Password("1212sos"))
  println(player)
  println(newPlayer)
  println(pla.asJson)

  val jsonPLayer = parse(player).getOrElse(Json.Null)
  val jsonNewPlayer = parse(newPlayer).getOrElse(Json.Null)
  println(jsonPLayer)
  println(jsonNewPlayer)
  val newPlayerJson = jsonNewPlayer.as[NewPlayer]
  val playerJson = jsonPLayer.as[Player]
  println(newPlayerJson)
  println(playerJson)
}
