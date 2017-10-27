package esmonad

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.scalatest._

class V3Spec_2 extends AsyncFlatSpec with Matchers {
  import esmonad.V3_2._

  "The V3_2 object" should "be valid" in {

    (
      for {
        events <- EitherT.fromEither(Turtle.create("123", Position.zero, North).map(Seq(_)))
        _ <- EitherT.right(persist(events))
        state <- OptionT(hydrate[Turtle]("123")).toRight("not found")
        moreEvents <- EitherT.fromEither(many[Turtle, TurtleEvent](state, Seq(
          Turtle.walk(1),
          Turtle.turn(ToRight),
          Turtle.walk(1),
          Turtle.turn(ToRight),
          Turtle.walk(2),
          Turtle.turn(ToRight),
          Turtle.walk(2),
        ))(Turtle.handler).map { case (evts, _) => evts })
        _ <- EitherT.right(persist(moreEvents))

        state3 <- OptionT(hydrate[Turtle]("123")).toRight("not found")
      } yield state3
    ).value.map {
      _ shouldBe Right(Turtle("123", Position(-1, -1), West))
    }

  }

}
