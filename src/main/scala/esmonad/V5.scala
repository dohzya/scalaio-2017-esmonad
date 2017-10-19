package esmonad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object V5App extends V5 with App

trait V5 {

  // The state

  sealed trait Direction
  case object North extends Direction; case object South extends Direction
  case object Est extends Direction; case object West extends Direction

  case class Turtle(id: String, x: Int, y: Int, dir: Direction)

  // The events

  sealed trait TurtleEvent { def id: String }
  case class Create(id: String) extends TurtleEvent
  case class Look(id: String, dir: Direction) extends TurtleEvent
  case class Forward(id: String, amount: Int) extends TurtleEvent

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
  val handler = EventHandler[Turtle, TurtleEvent] {
    case (None, Create(id)) => Turtle(id, 0, 0, North)
    case (Some(t), Look(id, dir)) if id == t.id => t.copy(dir = dir)
    case (Some(t), Forward(id, a)) if id == t.id => t.dir match {
      case North => t.copy(y = t.y + a); case South => t.copy(y = t.y - a)
      case Est => t.copy(x = t.x + a); case West => t.copy(x = t.x - a)
    }
  }

  type CommandHandler[STATE, COMMAND, ERROR, EVENT] =
    (STATE, COMMAND) => Either[ERROR, EVENT]

  object Turtle {
    def create(id: String): Either[String, TurtleEvent] =
      Right(Create(id))

    def look(id: String, direction: Direction): Either[String, TurtleEvent] =
      Right(Look(id, direction))

    def forward(id: String, amount: Int): Either[String, TurtleEvent] =
      if (amount < 0) Left("Unable to forward of a negative amount")
      else Right(Forward(id, amount))
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
      journal(id).map { events => events.foldLeft(Option.empty[Turtle])(handler) }

  }

  def hydrate[STATE : Hydratable](id: String): Future[Option[STATE]] =
    implicitly[Hydratable[STATE]].hydrate(id)

  def persist[EVENT : WriteJournal](event: EVENT): Future[Unit] =
    implicitly[WriteJournal[EVENT]].write(event)

}
