package esmonad

import scala.concurrent.Future

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.scalatest._

class V6Spec extends AsyncFlatSpec with Matchers {

  "The V6 object" should "be valid" in {

    import V6App._

    (
      for {
        _ <- EitherT.fromEither[Future](Right(()))

        event1 = Create("123")
        _ <- EitherT.right(persist(event1))

        state1 <- OptionT(hydrate[Turtle]("123")).toRight("not found")
        event2 = Look(state1.id, South)
        state2 <- EitherT.fromEither(handler(Some(state1), event2))
        event3 = Forward(state2.id, 2)
        state3 <- EitherT.fromEither(handler(Some(state2), event3))
        event4 = Look(state3.id, West)
        _ <- EitherT.right(persist(event2))
        _ <- EitherT.right(persist(event3))
        _ <- EitherT.right(persist(event4))

        state3 <- OptionT(hydrate[Turtle]("123")).toRight("not found")
      } yield state3
    ).value.map {
      _ shouldBe Right(Turtle("123", 0, -2, West))
    }

  }

}
