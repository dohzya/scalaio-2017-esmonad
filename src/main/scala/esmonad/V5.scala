package esmonad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object V5App extends V5 with App

trait V5 { // Complete Event Sourced

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

    def turn(turtle: Turtle, rot: Rotation): Either[String, TurtleEvent] =
      Right(Turn(turtle.id, rot))

    def walk(turtle: Turtle, dist: Int): Either[String, TurtleEvent] = {
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

  def hydrate[STATE : Hydratable](id: String): Future[Option[STATE]] =
    implicitly[Hydratable[STATE]].hydrate(id)

  def persist[EVENT : WriteJournal](events: Seq[EVENT]): Future[Unit] =
    implicitly[WriteJournal[EVENT]].write(events)

}
