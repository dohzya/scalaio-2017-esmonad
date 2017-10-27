package esmonad

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.scalatest._

class V3Spec extends AsyncFlatSpec with Matchers {
  import esmonad.V3._

  "The V3 object" should "be valid" in {

    (
      for {
        events <- EitherT.fromEither(
          //for {
          //  (state1, event1) <- Turtle.create("123", Position.zero, North)
          //  (state2, event2) <- Turtle.walk(1)(state1)
          //  (state3, event3) <- Turtle.turn(ToRight)(state2)
          //  (state4, event4) <- Turtle.walk(1)(state3)
          //  (state5, event5) <- Turtle.turn(ToRight)(state4)
          //  (state6, event6) <- Turtle.walk(2)(state5)
          //  (state7, event7) <- Turtle.turn(ToRight)(state6)
          //  (state8, event8) <- Turtle.walk(2)(state7)
          //} yield Seq(
          //  event1, event2, event3, event4,
          //  event5, event6, event7, event8,
          //)
          Turtle.create("123", Position.zero, North).flatMap { case (state1, event1) =>
            Turtle.walk(1)(state1).flatMap { case (state2, event2) =>
              Turtle.turn(ToRight)(state2).flatMap { case (state3, event3) =>
                Turtle.walk(1)(state3).flatMap { case (state4, event4) =>
                  Turtle.turn(ToRight)(state4).flatMap { case (state5, event5) =>
                    Turtle.walk(2)(state5).flatMap { case (state6, event6) =>
                      Turtle.turn(ToRight)(state6).flatMap { case (state7, event7) =>
                        Turtle.walk(2)(state7).map { case (_, event8) =>
                          Seq(
                            event1, event2, event3, event4,
                            event5, event6, event7, event8,
                          )
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        )
        _ <- EitherT.right(persist(events))

        state3 <- OptionT(hydrate[Turtle]("123")).toRight("not found")
      } yield state3
    ).value.map {
      _ shouldBe Right(Turtle("123", Position(-1, -1), West))
    }

  }

}
