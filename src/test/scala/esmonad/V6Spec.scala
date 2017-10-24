package esmonad

import scala.concurrent.Future

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.scalatest._

class V6Spec extends AsyncFlatSpec with Matchers {
  import esmonad.V6App._

  "The V6 object" should "be valid" in {

    (
      for {
        events <- EitherT.fromEither(
          Act.empty[Turtle, TurtleEvent](
            Turtle.handler,
            Turtle.create("123", Position.zero, North)
          ) and
          Turtle.walk(1) and
          Turtle.turn(ToRight) and
          Turtle.walk(1) and
          Turtle.turn(ToRight) and
          Turtle.walk(2) and
          Turtle.turn(ToRight) and
          Turtle.walk(2) events
        )
        _ <- EitherT.right(persist(events))

        state3 <- OptionT(hydrate[Turtle]("123")).toRight("not found")
      } yield state3
    ).value.map {
      _ shouldBe Right(Turtle("123", Position(-1, -1), West))
    }

  }

}
