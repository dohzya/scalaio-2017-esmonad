package esmonad

import cats.data.{EitherT, OptionT}
import cats.instances.either._
import cats.instances.future._
import org.scalatest.{AsyncFlatSpec, Matchers}
import scala.concurrent.Future

class V6Spec extends AsyncFlatSpec with Matchers {
  //import esmonad.V6._
  //import esmonad.V6_2._
  import esmonad.V6_3._
  import Sourced._

  "The V6 object" should "be valid" in {

    def walkRight(dist: Int)(state: Turtle) =
      for {
        state1 <- source(state, Turtle.walk(dist))
        state2 <- source(state1, Turtle.turn(ToRight))
      } yield state2

    {
      for {
        events <- EitherT.fromEither[Future](
          sourceNew[Turtle](Turtle.create("123", Position.zero, North))
            .flatMap(walkRight(1))
            .flatMap(walkRight(1))
            .flatMap(source(Turtle.walk(2)))
            .events
        )
        _ <- EitherT.right(persist(events))

        state1 <- OptionT(hydrate[Turtle]("123")).toRight("not found")
        moreEvents <- EitherT.fromEither[Future](
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
