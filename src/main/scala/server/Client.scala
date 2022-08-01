package server

//import cats.effect.{ExitCode, IO, IOApp, Resource}
//import io.circe.Json
//import org.http4s.Method.POST
//import org.http4s._
//import org.http4s.client.blaze.BlazeClientBuilder
//import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSFrame, WSRequest}
//import org.http4s.implicits.http4sLiteralsSyntax
//
//import java.net.http.HttpClient
//import scala.concurrent.ExecutionContext
//import scala.io.StdIn
//import CommonClasses._
//import cats.implicits.catsSyntaxFlatMapOps


//object Client extends IOApp {
//
//  import MessageJson._
//  import org.http4s.circe.CirceEntityCodec._
//
//  private val uri = uri"http://localhost:9001/registration"
//
//  private def printLine(string: String = ""): IO[Unit] = IO(println(string))
//
//  private def clientResource(token: Token): Resource[IO, WSConnectionHighLevel[IO]] = Resource.eval(IO(HttpClient.newHttpClient()))
//    .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri"ws://127.0.0.1:9001/message/masana")))
//
//  private val tok = Token("115a706d-d602-4960-ab58-e7064e41a7a7&fsdfsdfsdfsdf")
//
//
//  override def run(args: List[String]): IO[ExitCode] = {
//    for {
////    tok <- BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
////      for {
////      _ <- printLine("Im Born. Enter your message: 1. NewPlayer, 2. LogIn")
////        message = StdIn.readLine().trim match {
////          case "1" => "NewPlayer"
////          case "2" => "LogIn"
////          case _ => throw new Exception("Wrong message")
////        }
////        _ <- printLine(message)
////        _ <- printLine("Enter your login")
////        login = StdIn.readLine().trim
////        _ <- printLine("Enter your password")
////        password = StdIn.readLine().trim
////        json = Json.obj("message" -> Json.fromString(message), "login" -> Json.fromString(login), "password" -> Json.fromString(password))
////        request = Request[IO](method = POST, uri).withEntity(json)
////        token <- client.fetchAs[Token](request)
////        _ <- printLine(token.toString)
////      } yield token
////    }
////    _ <- printLine("Tyta")
//      _ <- clientResource(tok).use { conn =>
//        for {
//          _ <- printLine(tok.toString)
//          _ <- conn.send(WSFrame.Text("balance"))
//          _ <- conn.receiveStream.collectFirst{
//            case WSFrame.Text(s, _) => s
//          }.compile.string >>= printLine
//        } yield ()
//      }
//      } yield ExitCode.Success
//  }
//}
