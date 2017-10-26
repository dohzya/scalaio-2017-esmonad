package esmonad

// syntactic sugar on handler usage
object V2_2 extends V2Models {
  type EventHandler[STATE, EVENT] = (Option[STATE], EVENT) => Some[STATE]

  val handler: EventHandler[Turtle, TurtleEvent] = {
    case (None, Create(id, pos, dir)) =>
      Some(Turtle(id, pos, dir))
    case (Some(t), Turn(id, rot)) if id == t.id =>
      Some(t.copy(dir = Direction.rotate(t.dir, rot)))
    case (Some(t), Walk(id, dist)) if id == t.id =>
      Some(t.copy(pos = Position.move(t.pos, t.dir, dist)))
    case (event, state) =>
      sys.error(s"Invalid event $event for state $state")
  }

}
