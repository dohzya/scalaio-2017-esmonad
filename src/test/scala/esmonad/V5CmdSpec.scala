package esmonad

import cats.data.EitherT
import cats.instances.future._
import org.scalatest._
import scala.concurrent.Future


class V5CmdSpec extends AsyncFlatSpec with Matchers {

  "The V5 object" should "be valid" in {

    import V5CmdApp._

    val id = "123"
    (
      for {
        _ <- EitherT.fromEither[Future](Right(()))

        cmd1 = Create(id)
        event1 <- EitherT.fromEither(commandHandler(None, cmd1))
        _ <- EitherT.right(persist(event1))

        state1 <- EitherT.fromOptionF(hydrate[Turtle]("123"), "Not found")
        cmd2 = Look(id, South)
        event2 <- EitherT.fromEither(commandHandler(Some(state1), cmd2))
        Some(state2) = eventHandler(Some(state1), event2)
        cmd3 = Forward(id, 2)
        event3 <- EitherT.fromEither(commandHandler(Some(state2), cmd3))
        Some(state3) = eventHandler(Some(state2), event3)
        cmd4 = Look(id, West)
        event4 <- EitherT.fromEither(commandHandler(Some(state3), cmd4))
        _ <- EitherT.right(persist(event2))
        _ <- EitherT.right(persist(event3))
        _ <- EitherT.right(persist(event4))

        state4 <- EitherT.fromOptionF(hydrate[Turtle]("123"), "Not found")
      } yield state4
    ).value.map {
       _ shouldBe Right(Turtle("123", 0, -2, West))
    }

  }

}
