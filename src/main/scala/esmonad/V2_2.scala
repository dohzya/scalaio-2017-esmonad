package esmonad

// syntactic sugar on handler usage
object V2_2 extends V2Models {
  type EventHandler[STATE, EVENT] = (Option[STATE], EVENT) => Some[STATE]

  val handler: EventHandler[Turtle, TurtleEvent] = {
    case (None, Create(id, pos, dir)) =>
      Some(Turtle(id, pos, dir))
    case (Some(t), Turn(id, rot)) if id == t.id =>
      Some(t.copy(dir = t.dir.rotate(rot)))
    case (Some(t), Walk(id, dist)) if id == t.id =>
      Some(t.copy(pos = t.pos.move(t.dir, dist)))
    case (state, event) =>
      sys.error(s"Invalid event $event for state $state")
  }

}
