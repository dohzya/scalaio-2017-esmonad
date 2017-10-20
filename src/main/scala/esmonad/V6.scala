package esmonad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object V6App extends V6 with App

trait V6 {

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

  case class EventHandler[STATE, EVENT, ERROR](
    defaultError: (Option[STATE], EVENT) => ERROR
  )(
    fn: PartialFunction[(Option[STATE], EVENT), Either[ERROR, STATE]]
  ) extends Function2[Option[STATE], EVENT, Either[ERROR, STATE]] {
    def apply(state: Option[STATE], event: EVENT): Either[ERROR, STATE] = {
      val input = (state, event)
      if (fn.isDefinedAt(input)) fn(input)
      else Left(defaultError(state, event))
    }
  }

  // The handler
  val handler = EventHandler[Turtle, TurtleEvent, String](
    (state, event) => sys.error(s"Invalid event $event for state $state")
  ) {
    case (None, Create(id)) => Right(Turtle(id, 0, 0, North))
    case (Some(t), Look(id, dir)) if id == t.id => Right(t.copy(dir = dir))
    case (Some(t), Forward(id, a)) if id == t.id =>
      val forwarded = t.dir match {
        case North => t.copy(y = t.y + a); case South => t.copy(y = t.y - a)
        case Est => t.copy(x = t.x + a); case West => t.copy(x = t.x - a)
      }
      if (forwarded.x.abs > 100 || forwarded.y.abs > 100) Right(forwarded)
      else Left("Can't go too far away")
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
      journal(id).map { events =>
        events.foldLeft(Option.empty[Turtle]) { (state, event) =>
          handler(state, event).fold(
            err => sys.error(err),
            state => Some(state)
          )
        }
      }

  }

  def hydrate[STATE : Hydratable](id: String): Future[Option[STATE]] =
    implicitly[Hydratable[STATE]].hydrate(id)

  def persist[EVENT : WriteJournal](event: EVENT): Future[Unit] =
    implicitly[WriteJournal[EVENT]].write(event)

}
