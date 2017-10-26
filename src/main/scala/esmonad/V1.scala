package esmonad

import scala.language.higherKinds

trait V1Events {

  sealed trait TurtleEvent
  case class Turn(rot: Rotation) extends TurtleEvent
  case class Walk(dist: Int) extends TurtleEvent

}

trait V1Models extends V1Events {

  type EventHandler[STATE, EVENT] = (STATE, EVENT) => STATE

  case class Turtle(id: String, pos: Position, dir: Direction)

  object Turtle {
    implicit val handler: EventHandler[Turtle, TurtleEvent] = {
      case (state, Turn(rot)) => state.copy(dir = state.dir.rotate(rot))
      case (state, Walk(dist)) => state.copy(pos = state.pos.move(state.dir, dist))
    }
  }

}

object V1 extends V1Models
