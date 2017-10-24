package esmonad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object V6App extends V6 with App

trait V6Handler {
  case class EventHandler[STATE, EVENT](
    fn: PartialFunction[(Option[STATE], EVENT), STATE]
  ) extends ((Option[STATE], EVENT) => Some[STATE]) {
    def apply(state: Option[STATE], event: EVENT): Some[STATE] = {
      val input = (state, event)
      if (fn.isDefinedAt(input)) Some(fn(input))
      else sys.error(s"Invalid event $event for state $state")
    }
  }
}

trait V6Journal { this: V6Handler =>
  trait WriteJournal[EVENT] {
    def write(events: Seq[EVENT]): Future[Unit]
  }

  trait Hydratable[STATE] {
    def hydrate(id: String): Future[Option[STATE]]
  }

  class DefaultJournal[STATE, EVENT](handler: EventHandler[STATE, EVENT], eventID: EVENT => String) extends WriteJournal[EVENT] with Hydratable[STATE] {
    private var journal = Seq.empty[EVENT]

    override def write(event: Seq[EVENT]): Future[Unit] = Future {
      synchronized {
        journal = journal ++ event
      }
    }
    def journal(id: String): Future[Seq[EVENT]] = Future {
      synchronized {
        journal.filter(event => eventID(event) == id)
      }
    }
    override def hydrate(id: String): Future[Option[STATE]] =
      journal(id).map { events => events.foldLeft(Option.empty[STATE])(handler) }

  }

  def hydrate[STATE : Hydratable](id: String): Future[Option[STATE]] =
    implicitly[Hydratable[STATE]].hydrate(id)

  def persist[EVENT : WriteJournal](events: Seq[EVENT]): Future[Unit] =
    implicitly[WriteJournal[EVENT]].write(events)
}

trait V6Model { this: V6Handler with V6Journal =>
  case class Turtle(id: String, pos: Position, dir: Direction)

  sealed trait TurtleEvent { def id: String }
  case class Create(id: String, pos: Position, dir: Direction)
    extends TurtleEvent
  case class Turn(id: String, rot: Rotation) extends TurtleEvent
  case class Walk(id: String, dist: Int) extends TurtleEvent

  type TurtleError = String

  object Turtle {
    def create(id: String, pos: Position, dir: Direction): Either[TurtleError, TurtleEvent] =
      Right(Create(id, pos, dir))

    def turn(rot: Rotation)(turtle: Turtle): Either[TurtleError, TurtleEvent] =
      Right(Turn(turtle.id, rot))

    def walk(dist: Int)(turtle: Turtle): Either[TurtleError, TurtleEvent] = {
      val moved = Position.move(turtle.pos, turtle.dir, dist)
      if (moved.x.abs > 100 || moved.y.abs > 100) Left("Too far away")
      else Right(Walk(turtle.id, dist))
    }

    // The handler
    val handler = EventHandler[Turtle, TurtleEvent] {
      case (None, Create(id, pos, dir)) => Turtle(id, pos, dir)
      case (Some(t), Turn(id, rot)) if id == t.id =>
        t.copy(dir = Direction.rotate(t.dir, rot))
      case (Some(t), Walk(id, dist)) if id == t.id =>
        t.copy(pos = Position.move(t.pos, t.dir, dist))
    }
  }

  implicit object TurtleJournal extends DefaultJournal[Turtle, TurtleEvent](Turtle.handler, _.id)

}

trait V6 extends V6Handler with V6Journal with V6Model { // Better syntax

  case class Act[STATE, EVENT](
    handler: (Option[STATE], EVENT) => Some[STATE],
    value: Either[String, (Seq[EVENT], STATE)],
  ) {
    def events = value.map { case (events, _) => events }
    def and(fn: STATE => Either[String, EVENT]): Act[STATE, EVENT] = {
      Act(handler, value.flatMap { case (events, state) =>
        fn(state).map { newEvent =>
          val Some(newState) = handler(Some(state), newEvent)
          (events :+ newEvent, newState)
        }
      })
    }
  }
  object Act {
    def empty[STATE, EVENT](
      handler: (Option[STATE], EVENT) => Some[STATE],
      value: Either[String, EVENT]
    ): Act[STATE, EVENT] = {
      Act(handler, value.map { event =>
        val Some(state) = handler(None, event)
        (Seq(event), state)
      })
    }
  }

}
