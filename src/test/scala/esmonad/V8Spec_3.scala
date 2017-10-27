package esmonad

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.scalatest.{AsyncFlatSpec, Matchers}

class V8Spec_3 extends AsyncFlatSpec with Matchers {
  import esmonad.V8_3._
  import Sourced._

  "The V8 object" should "be valid" in {

    def walkRight(dist: Int): SourcedUpdate[Turtle, TurtleEvent, Unit] =
      for {
        _ <- source(Turtle.walk(dist))
        _ <- source(Turtle.turn(ToRight))
      } yield ()

    {
      for {
        events <- EitherT.fromEither(
          sourceNew[Turtle](Turtle.create("123", Position.zero, North)).andThen(_ =>
            for {
              _ <- walkRight(1)
              _ <- walkRight(1)
              _ <- when[Turtle](_.dir == North, Turtle.walk(1))
              _ <- source(Turtle.walk(2))
            } yield ()
          ).events
        )
        _ <- EitherT.right(persist(events))

        state1 <- OptionT(hydrate[Turtle]("123")).toRight("not found")
        moreEvents <- EitherT.fromEither(
          (for {
            _ <- source(state1, Turtle.turn(ToRight))
            _ <- source(Turtle.walk(2))
          } yield ()).events(state1)
        )
        _ <- EitherT.right(persist(moreEvents))

        state3 <- OptionT(hydrate[Turtle]("123")).toRight("not found")
      } yield state3
    }.value.map {
      _ shouldBe Right(Turtle("123", Position(-1, -1), West))
    }

  }

}
