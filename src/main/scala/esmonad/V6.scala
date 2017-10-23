package esmonad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object V6App extends V6 with App

trait V6 { // Better syntax

  case class Turtle(id: String, pos: Position, dir: Direction)

  sealed trait TurtleEvent { def id: String }
  case class Create(id: String, pos: Position, dir: Direction)
    extends TurtleEvent
  case class Turn(id: String, rot: Rotation) extends TurtleEvent
  case class Walk(id: String, dist: Int) extends TurtleEvent

  case class EventHandler[STATE, EVENT](
    fn: PartialFunction[(Option[STATE], EVENT), STATE]
  ) extends ((Option[STATE], EVENT) => Some[STATE]) {
    def apply(state: Option[STATE], event: EVENT): Some[STATE] = {
      val input = (state, event)
      if (fn.isDefinedAt(input)) Some(fn(input))
      else sys.error(s"Invalid event $event for state $state")
    }
  }

  // The handler
  val handler = EventHandler[Turtle, TurtleEvent] {
    case (None, Create(id, pos, dir)) => Turtle(id, pos, dir)
    case (Some(t), Turn(id, rot)) if id == t.id =>
      t.copy(dir = Direction.rotate(t.dir, rot))
    case (Some(t), Walk(id, dist)) if id == t.id =>
      t.copy(pos = Position.move(t.pos, t.dir, dist))
  }

  type CommandHandler[STATE, COMMAND, ERROR, EVENT] =
    (STATE, COMMAND) => Either[ERROR, EVENT]

  object Turtle {
    def create(id: String, pos: Position, dir: Direction): Either[String, TurtleEvent] =
      Right(Create(id, pos, dir))

    def turn(rot: Rotation)(turtle: Turtle): Either[String, TurtleEvent] =
      Right(Turn(turtle.id, rot))

    def walk(dist: Int)(turtle: Turtle): Either[String, TurtleEvent] = {
      val moved = Position.move(turtle.pos, turtle.dir, dist)
      if (moved.x.abs > 100 || moved.y.abs > 100) Left("Too far away")
      else Right(Walk(turtle.id, dist))
    }
  }

  trait WriteJournal[EVENT] {
    def write(events: Seq[EVENT]): Future[Unit]
  }

  trait Hydratable[STATE] {
    def hydrate(id: String): Future[Option[STATE]]
  }

  implicit object TurtleJournal extends WriteJournal[TurtleEvent] with Hydratable[Turtle] {
    private var journal = Seq.empty[TurtleEvent]

    override def write(event: Seq[TurtleEvent]): Future[Unit] = Future {
      synchronized {
        journal = journal ++ event
      }
    }
    def journal(id: String): Future[Seq[TurtleEvent]] = Future {
      synchronized {
        journal.filter(_.id == id)
      }
    }
    override def hydrate(id: String): Future[Option[Turtle]] =
      journal(id).map { events => events.foldLeft(Option.empty[Turtle])(handler) }

  }

  def hydrate[STATE : Hydratable](id: String): Future[Option[STATE]] =
    implicitly[Hydratable[STATE]].hydrate(id)

  def persist[EVENT : WriteJournal](events: Seq[EVENT]): Future[Unit] =
    implicitly[WriteJournal[EVENT]].write(events)

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
