package esmonad

trait V1Handler {

  type EventHandler[STATE, EVENT] = (STATE, EVENT) => STATE

}

trait V1Models { self: V1Handler => // Basic Event Sourced

  case class Turtle(id: String, pos: Position, dir: Direction)

  sealed trait TurtleEvent
  case class Turn(rot: Rotation) extends TurtleEvent
  case class Walk(dist: Int) extends TurtleEvent

  object Turtle {
    val handler: EventHandler[Turtle, TurtleEvent] = {
      case (state, Turn(rot)) => state.copy(dir = Direction.rotate(state.dir, rot))
      case (state, Walk(dist)) => state.copy(pos = Position.move(state.pos, state.dir, dist))
    }
  }

}

object V1 extends V1Models with V1Handler
