package Game

import io.circe.Json
import io.circe.parser._
import server.Protocol._
import server.MessageJson._

object RPG extends App{
  val mail = """{"mail":"dsdsd@mail.ru"}"""
  val password = """{"password": "mig943g"}"""
  val login = """{"login": "masana"}"""
  val player = s"""{"login": $login, "password": $password}"""
  val newPlayer = s"""{"mail": $mail, "player": $player}"""
  println(player)
  println(newPlayer)

  val jsonPLayer = parse(player).getOrElse(Json.Null)
  val jsonNewPlayer = parse(newPlayer).getOrElse(Json.Null)
  println(jsonPLayer)
  println(jsonNewPlayer)
  val newPlayerJson = jsonNewPlayer.as[NewPlayer]
  val playerJson = jsonPLayer.as[Player]
  println(newPlayerJson)
  println(playerJson)
}
