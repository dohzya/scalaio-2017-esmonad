package esmonad

trait V2Handler {

  type EventHandler[STATE, EVENT] = (Option[STATE], EVENT) => STATE

}

trait V2Models { self: V2Handler => // Event Sourced (with creation event)

  case class Turtle(id: String, pos: Position, dir: Direction)

  sealed trait TurtleEvent
  case class Create(id: String, pos: Position, dir: Direction)
    extends TurtleEvent
  case class Turn(id: String, rot: Rotation) extends TurtleEvent
  case class Walk(id: String, dist: Int) extends TurtleEvent

  object Turtle {
    val handler: EventHandler[Turtle, TurtleEvent] = {
      case (None, Create(id, pos, dir)) => Turtle(id, pos, dir)
      case (Some(t), Turn(id, rot)) if id == t.id =>
        t.copy(dir = Direction.rotate(t.dir, rot))
      case (Some(t), Walk(id, dist)) if id == t.id =>
        t.copy(pos = Position.move(t.pos, t.dir, dist))
      case (event, state) => sys.error(s"Invalid event $event for state $state")
    }
  }

}

object V2 extends V2Models with V2Handler
