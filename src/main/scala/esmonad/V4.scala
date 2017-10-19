package esmonad

object V4App extends V4 with App

trait V4 {

  // The state

  sealed trait Direction
  case object North extends Direction; case object South extends Direction
  case object Est extends Direction; case object West extends Direction

  case class Turtle(id: String, x: Int, y: Int, dir: Direction)

  // The events

  sealed trait TurtleEvent
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

}
