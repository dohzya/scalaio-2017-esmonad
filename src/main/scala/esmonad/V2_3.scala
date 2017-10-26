package esmonad

trait FinalHandlers {

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

// Factory for EventHandler
object V2_3 extends V2Models with FinalHandlers{

  val handler = EventHandler[Turtle, TurtleEvent] {
    case (None, Create(id, pos, dir)) => Turtle(id, pos, dir)
    case (Some(t), Turn(id, rot)) if id == t.id =>
      t.copy(dir = t.dir.rotate(rot))
    case (Some(t), Walk(id, dist)) if id == t.id =>
      t.copy(pos = t.pos.move(t.dir, dist))
  }
}
