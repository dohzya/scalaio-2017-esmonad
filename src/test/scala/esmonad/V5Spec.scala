package esmonad

import scala.concurrent.Future

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.scalatest._

class V5Spec extends AsyncFlatSpec with Matchers {
  import esmonad.V5._

  "The V6 object" should "be valid" in {

    (
      for {
        events <- EitherT.fromEither(
          for {
            event1 <- Turtle.create("123", Position.zero, North)
            Some(state1) = Turtle.handler(None, event1)
            event2 <- Turtle.walk(1)(state1)
            Some(state2) = Turtle.handler(Some(state1), event2)
            event3 <- Turtle.turn(ToRight)(state2)
            Some(state3) = Turtle.handler(Some(state2), event3)
            event4 <- Turtle.walk(1)(state3)
            Some(state4) = Turtle.handler(Some(state3), event4)
            event5 <- Turtle.turn(ToRight)(state4)
            Some(state5) = Turtle.handler(Some(state4), event5)
            event6 <- Turtle.walk(2)(state5)
            Some(state6) = Turtle.handler(Some(state5), event6)
            event7 <- Turtle.turn(ToRight)(state6)
            Some(state7) = Turtle.handler(Some(state6), event7)
            event8 <- Turtle.walk(2)(state7)
          } yield Seq(
            event1, event2, event3, event4,
            event5, event6, event7, event8,
          )
        )
        _ <- EitherT.right(persist(events))

        state3 <- OptionT(hydrate[Turtle]("123")).toRight("not found")
      } yield state3
    ).value.map {
      _ shouldBe Right(Turtle("123", Position(-1, -1), West))
    }

  }

}
