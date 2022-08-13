package server

import server.service.Doobie._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import ForDoobieTest._
import cats.effect.testing.scalatest.AsyncIOSpec
import server.models.Protocol.{Login, Mail, NewPlayer, Password, Player}

class DoobieTest extends AsyncFreeSpec with Matchers with AsyncIOSpec{


  "Doobie should" - {
    "create player" in {
      createPlayer(playerTest).asserting(_ shouldBe ())
    }

    "verify player" in {
      verifyPlayer(playerTest).asserting(_ shouldBe ())
    }

    "get balance" in {
      getBalance(playerTest.login).asserting(_ shouldBe 100000)
    }

    "update balance" in {
      val action = for {
        _ <- updateBalance(100, playerTest.login)
        amount <- getBalance(playerTest.login)
      } yield amount
      action.asserting(_ shouldBe 100100)
    }

    "delete player" in {
      deletePlayer(playerTest.login).asserting(_ shouldBe ())
    }
  }
}

object ForDoobieTest {
  val loginTest: Login = Login("testLogin")
  val passwordTest: Password = Password("testPassword")
  val mailTest: Mail = Mail("test@mail.ru")
  val playerTest: Player = Player(loginTest, passwordTest)
  val newPlayerTest: NewPlayer = NewPlayer(mailTest, playerTest)
}
