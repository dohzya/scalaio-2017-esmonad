package esmonad

object V1App extends V1 with App

trait V1 { // Basic Event Sourced

  case class Turtle(id: String, pos: Position, dir: Direction)

  sealed trait TurtleEvent
  case class Turn(rot: Rotation) extends TurtleEvent
  case class Walk(dist: Int) extends TurtleEvent

  type EventHandler[STATE, EVENT] = (STATE, EVENT) => STATE

  val handler: EventHandler[Turtle, TurtleEvent] = {
    case (state, Turn(rot)) => state.copy(dir = Direction.rotate(state.dir, rot))
    case (state, Walk(dist)) => state.copy(pos = Position.move(state.pos, state.dir, dist))
  }

}
