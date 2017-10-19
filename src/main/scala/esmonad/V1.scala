package esmonad

object V1App extends V1 with App

trait V1 {

  // The state

  sealed trait Direction
  case object North extends Direction; case object South extends Direction
  case object Est extends Direction; case object West extends Direction

  case class Turtle(x: Int, y: Int, dir: Direction)

  // The events

  sealed trait TurtleEvent
  case class Look(dir: Direction) extends TurtleEvent
  case class Forward(amount: Int) extends TurtleEvent

  type EventHandler[STATE, EVENT] = (STATE, EVENT) => STATE

  // The handler
  def handler(state: Turtle, event: TurtleEvent): Turtle =
    event match {
      case Look(dir) => state.copy(dir = dir)
      case Forward(a) => state.dir match {
        case North => state.copy(y = state.y + a); case South => state.copy(y = state.y - a)
        case Est => state.copy(x = state.x + a); case West => state.copy(x = state.x - a)
      }
  }

}
