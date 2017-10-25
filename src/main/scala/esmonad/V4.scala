package esmonad

trait V4Handler {

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

trait V4Models { self: V4Handler => // Factory for EventHandler

  case class Turtle(id: String, pos: Position, dir: Direction)

  sealed trait TurtleEvent
  case class Create(id: String, pos: Position, dir: Direction)
    extends TurtleEvent
  case class Turn(id: String, rot: Rotation) extends TurtleEvent
  case class Walk(id: String, dist: Int) extends TurtleEvent

  object Turtle {
    val handler = EventHandler[Turtle, TurtleEvent] {
      case (None, Create(id, pos, dir)) => Turtle(id, pos, dir)
      case (Some(t), Turn(id, rot)) if id == t.id =>
        t.copy(dir = Direction.rotate(t.dir, rot))
      case (Some(t), Walk(id, dist)) if id == t.id =>
        t.copy(pos = Position.move(t.pos, t.dir, dist))
    }
  }

}

object V4 extends V4Models with V4Handler
