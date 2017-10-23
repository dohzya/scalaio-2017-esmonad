package esmonad

import scala.concurrent.Future

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import org.scalatest._

class V5Spec extends AsyncFlatSpec with Matchers {
  import esmonad.V5App._

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
          for {
            event1 <- Turtle.create("123", Position.zero, North)
            Some(state1) = handler(None, event1)
            event2 <- Turtle.walk(state1, 1)
            Some(state2) = handler(Some(state1), event2)
            event3 <- Turtle.turn(state2, ToRight)
            Some(state3) = handler(Some(state2), event3)
            event4 <- Turtle.walk(state3, 1)
            Some(state4) = handler(Some(state3), event4)
            event5 <- Turtle.turn(state4, ToRight)
            Some(state5) = handler(Some(state4), event5)
            event6 <- Turtle.walk(state5, 2)
            Some(state6) = handler(Some(state5), event6)
            event7 <- Turtle.turn(state6, ToRight)
            Some(state7) = handler(Some(state6), event7)
            event8 <- Turtle.walk(state7, 2)
          } yield Seq(
            event1, event2, event3, event4,
            event5, event6, event7, event8,
          )
        )
        _ <- EitherT.right(persist(events))

        state3 <- OptionT(hydrate[Turtle]("123")).toRight("not found")
      } yield state3
    ).value.map {
      _ shouldBe Right(Turtle("123", Position(-1, -1), West))
    }

  }

}

class V5BisSpec extends AsyncFlatSpec with Matchers {
  import esmonad.V5App._

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

  def act(block: Either[String, TurtleEvent]): Either[String, (TurtleEvent, Turtle)] = {
    block.map { event =>
      val Some(newState) = handler(None, event)
      (event, newState)
    }
  }
  def act(state: Turtle, block: Turtle => Either[String, TurtleEvent]): Either[String, (TurtleEvent, Turtle)] = {
    block(state).map { event =>
      val Some(newState) = handler(Some(state), event)
      (event, newState)
    }
  }

  "The V5 object" should "be valid" in {

    assertDoesNotCompile(
      """
        |import V6App._
        |for {
        |  (event1, state1) <- act(Turtle.create("123", Position.zero, North))
        |  (event2, state2) <- act(state1, Turtle.walk(1))
        |  (event3, state3) <- act(state2, Turtle.turn(ToRight))
        |  (event4, state4) <- act(state3, Turtle.walk(1))
        |  (event5, state5) <- act(state4, Turtle.turn(ToRight))
        |  (event6, state6) <- act(state5, Turtle.walk(2))
        |  (event7, state7) <- act(state6, Turtle.turn(ToRight))
        |  (event8, _)      <- act(state7, Turtle.walk(2))
        |} yield Seq(
        |  event1, event2, event3, event4,
        |  event5, event6, event7, event8,
        |)
      """.stripMargin)

  }

}
