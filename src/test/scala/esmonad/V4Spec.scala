package esmonad

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.scalatest.{AsyncFlatSpec, Matchers}

class V4Spec extends AsyncFlatSpec with Matchers {
  import esmonad.V4._
  //import esmonad.V4_2._
  import Sourced._

  "The V4 object" should "be valid" in {

    def walkRight(dist: Int)(state: Turtle) =
      for {
        state1 <- source(state, Turtle.walk(dist))
        state2 <- source(state1, Turtle.turn(ToRight))
      } yield state2

    {
      for {
        events <- EitherT.fromEither(
          sourceNew[Turtle](Turtle.create("123", Position.zero, North))
            .flatMap(walkRight(1))
            .flatMap(walkRight(1))
            .flatMap(source(Turtle.walk(2)))
            .events
        )
        _ <- EitherT.right(persist(events))

        state1 <- OptionT(hydrate[Turtle]("123")).toRight("not found")
        moreEvents <- EitherT.fromEither(
          source(state1, Turtle.turn(ToRight))
            .flatMap(source(Turtle.walk(2)))
            .events
        )
        _ <- EitherT.right(persist(moreEvents))

        state3 <- OptionT(hydrate[Turtle]("123")).toRight("not found")
      } yield state3
    }.value.map {
      _ shouldBe Right(Turtle("123", Position(-1, -1), West))
    }

  }

}
