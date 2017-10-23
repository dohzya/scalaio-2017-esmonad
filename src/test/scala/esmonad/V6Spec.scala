package esmonad

import scala.concurrent.Future

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.scalatest._

class V6Spec extends AsyncFlatSpec with Matchers {
  import esmonad.V6App._

  implicit object TurtleJournal extends WriteJournal[TurtleEvent] with Hydratable[Turtle] {
    private var journal = Seq.empty[TurtleEvent]
    override def write(event: Seq[TurtleEvent]): Future[Unit] = Future {
      synchronized { journal = journal ++ event }
    }
    def journal(id: String): Future[Seq[TurtleEvent]] = Future {
      synchronized { journal.filter(_.id == id) }
    }
    override def hydrate(id: String): Future[Option[Turtle]] =
      journal(id).map { events => events.foldLeft(Option.empty[Turtle])(handler) }
  }

  "The V6 object" should "be valid" in {

    (
      for {
        events <- EitherT.fromEither(
          Act.empty[Turtle, TurtleEvent](
            handler,
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
