package esmonad

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import cats.instances.either._
import org.scalatest.{AsyncFlatSpec, Matchers}

import scala.concurrent.Future

class V7Spec extends AsyncFlatSpec with Matchers {

  import esmonad.V7._
//  import esmonad.V7_2._
//  import esmonad.V7_3._
  import Sourced._

  "The V7 object" should "be valid" in {
    val id = "123"
    def walkRight = {
      source(Turtle.walk(1)) andThen
      source(Turtle.turn(ToRight))
    }

    (for {
      events <- EitherT.fromEither[Future] {
        (
          sourceNew[Turtle](Turtle.create(id, Position.zero, North)) andThen
          walkRight andThen
          walkRight andThen
          source(Turtle.walk(2))
        ).events
      }
      _ <- EitherT.right(persist(events))

      state1 <- OptionT(hydrate[Turtle](id)).toRight("not found")
      moreEvents <- EitherT.fromEither[Future] {
        Sourced.events(state1) { // alternative syntax
          source(Turtle.turn(ToRight)) andThen
          source(Turtle.walk(2))
        }
      }
      _ <- EitherT.right(persist(moreEvents))


      state2 <- OptionT(hydrate[Turtle](id)).toRight("not found")
    } yield state2
      ).value.map {
      _ shouldBe Right(Turtle("123", Position(-1, -1), West))
    }
  }

}
