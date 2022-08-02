package server

import io.circe.Json
import io.circe.parser.parse
import server.Protocol.{NewPlayer, Player}
import MessageJson._
import org.http4s.implicits.http4sLiteralsSyntax


object Polygon extends App{
  val string = s"""{"mail": {"value": "msaana23@mail.ru"}, "player":{"login": {"value": "masana"}, "password": {"value": "migsadas"}}}"""
  val str = uri"/message" / string
  println(parse(string).getOrElse(Json.Null).as[NewPlayer])
  println(str)
}
