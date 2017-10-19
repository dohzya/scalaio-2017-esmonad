package esmonad

object V3App extends V3 with App

trait V3 {

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

  type EventHandler[STATE, EVENT] = (Option[STATE], EVENT) => Some[STATE]

  // The handler
  def handler(state: Option[Turtle], event: TurtleEvent): Some[Turtle] =
    (state, event) match {
      case (None, Create(id)) => Some(Turtle(id, 0, 0, North))
      case (Some(t), Look(id, dir)) if id == t.id => Some(t.copy(dir = dir))
      case (Some(t), Forward(id, a)) if id == t.id => Some(t.dir match {
        case North => t.copy(y = t.y + a); case South => t.copy(y = t.y - a)
        case Est => t.copy(x = t.x + a); case West => t.copy(x = t.x - a)
      })
      case (state, event) => sys.error(s"Invalid event $event for state $state")
    }

}
