package esmonad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object V5CmdApp extends V5Cmd with App

trait V5Cmd {

  // The state

  sealed trait Direction
  case object North extends Direction; case object South extends Direction
  case object Est extends Direction; case object West extends Direction

  case class Turtle(id: String, x: Int, y: Int, dir: Direction)

  // The events

  sealed trait TurtleEvent { def id: String }
  case class Created(id: String) extends TurtleEvent
  case class Looked(id: String, dir: Direction) extends TurtleEvent
  case class Forwarded(id: String, amount: Int) extends TurtleEvent

  case class EventHandler[STATE, EVENT](
    fn: PartialFunction[(Option[STATE], EVENT), STATE]
  ) extends Function2[Option[STATE], EVENT, Some[STATE]] {
    def apply(state: Option[STATE], event: EVENT): Some[STATE] = {
      val input = (state, event)
      if (fn.isDefinedAt(input)) Some(fn(input))
      else sys.error(s"Invalid event $event for state $state")
    }
  }

  // The handler
  val eventHandler = EventHandler[Turtle, TurtleEvent] {
    case (None, Created(id)) => Turtle(id, 0, 0, North)
    case (Some(t), Looked(id, dir)) if id == t.id => t.copy(dir = dir)
    case (Some(t), Forwarded(id, a)) if id == t.id => t.dir match {
      case North => t.copy(y = t.y + a); case South => t.copy(y = t.y - a)
      case Est => t.copy(x = t.x + a); case West => t.copy(x = t.x - a)
    }
  }

  case class CommandHandler[STATE, COMMAND, ERROR, EVENT](
    defaultError: (Option[STATE], COMMAND) => ERROR
  )(
    fn: PartialFunction[(Option[STATE], COMMAND), Either[ERROR, EVENT]]
  ) extends Function2[Option[STATE], COMMAND, Either[ERROR, EVENT]] {
    def apply(state: Option[STATE], command: COMMAND): Either[ERROR, EVENT] = {
      val input = (state, command)
      if (fn.isDefinedAt(input)) fn(input)
      else Left(defaultError(state, command))
    }
  }

  sealed trait TurtleCommand { def id: String }
  case class Create(id: String) extends TurtleCommand
  case class Look(id: String, dir: Direction) extends TurtleCommand
  case class Forward(id: String, amount: Int) extends TurtleCommand

  val commandHandler = CommandHandler[Turtle, TurtleCommand, String, TurtleEvent](
    (state, command) => s"Invalid command $command for state $state"
  ) {
    case (None, Create(id)) => Right(Created(id))
    case (Some(turtle), Look(id, dir)) if turtle.id == id => Right(Looked(turtle.id, dir))
    case (Some(turtle), Forward(id, amount)) if turtle.id == id =>
      if (amount < 0) Left("Unable to forward of a negative amount")
      else Right(Forwarded(turtle.id, amount))
  }

  trait WriteJournal[EVENT] {
    def write(event: EVENT): Future[Unit]
  }

  trait Hydratable[STATE] {
    def hydrate(id: String): Future[Option[STATE]]
  }

  implicit object TurtleJournal extends WriteJournal[TurtleEvent] with Hydratable[Turtle] {
    private var journal = Seq.empty[TurtleEvent]

    override def write(event: TurtleEvent): Future[Unit] = Future {
      synchronized {
        journal = journal :+ event
      }
    }
    def journal(id: String): Future[Seq[TurtleEvent]] = Future {
      synchronized {
        journal.filter(_.id == id)
      }
    }
    override def hydrate(id: String): Future[Option[Turtle]] =
      journal(id).map { events => events.foldLeft(Option.empty[Turtle])(eventHandler) }

  }

  def hydrate[STATE : Hydratable](id: String): Future[Option[STATE]] =
    implicitly[Hydratable[STATE]].hydrate(id)

  def persist[EVENT : WriteJournal](event: EVENT): Future[Unit] =
    implicitly[WriteJournal[EVENT]].write(event)

}
