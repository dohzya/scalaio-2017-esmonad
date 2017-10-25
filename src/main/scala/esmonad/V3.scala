package esmonad

trait V3Handler {

  type EventHandler[STATE, EVENT] = (Option[STATE], EVENT) => Some[STATE]

}

trait V3Models { self: V3Handler => // syntactic sugar on handler usage

  case class Turtle(id: String, pos: Position, dir: Direction)

  sealed trait TurtleEvent
  case class Create(id: String, pos: Position, dir: Direction)
    extends TurtleEvent
  case class Turn(id: String, rot: Rotation) extends TurtleEvent
  case class Walk(id: String, dist: Int) extends TurtleEvent

  object Turtle {
    val handler: EventHandler[Turtle, TurtleEvent] = {
      case (None, Create(id, pos, dir)) => Some(Turtle(id, pos, dir))
      case (Some(t), Turn(id, rot)) if id == t.id =>
        Some(t.copy(dir = Direction.rotate(t.dir, rot)))
      case (Some(t), Walk(id, dist)) if id == t.id =>
        Some(t.copy(pos = Position.move(t.pos, t.dir, dist)))
      case (event, state) => sys.error(s"Invalid event $event for state $state")
    }
  }

}

object V3 extends V3Models with V3Handler
