package esmonad

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.scalatest.{AsyncFlatSpec, Matchers}

class V5Spec extends AsyncFlatSpec with Matchers {
  //import esmonad.V5._
  import esmonad.V5_2._
  import Sourced._

  "The V5 object" should "be valid" in {

    def walkRight(dist: Int) = {
      source(Turtle.walk(dist)) andThen
      source(Turtle.turn(ToRight))
    }

    (for {
      events <- EitherT.fromEither {
        (
          sourceNew[Turtle](Turtle.create("123", Position.zero, North)) andThen
          walkRight(1) andThen
          walkRight(1) andThen
          when[Turtle](_.dir == North, Turtle.walk(20)) andThen
          when[Turtle](_.dir == South, Turtle.walk(1)) andThen
          source(Turtle.walk(2))
        ).events
      }
      _ <- EitherT.right(persist(events))

      state1 <- OptionT(hydrate[Turtle]("123")).toRight("not found")
      moreEvents <- EitherT.fromEither {
        Sourced.events(state1) { // alternative syntax
          source(Turtle.turn(ToRight)) andThen
          source(Turtle.walk(2))
        }
      }
      _ <- EitherT.right(persist(moreEvents))


      state2 <- OptionT(hydrate[Turtle]("123")).toRight("not found")
    } yield state2
      ).value.map {
      _ shouldBe Right(Turtle("123", Position(-1, -2), West))
    }
  }

}
