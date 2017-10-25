package esmonad

trait V6Sourced { // Better syntax

  case class Sourced[STATE, EVENT](
    handler: (Option[STATE], EVENT) => Some[STATE],
    value: Either[String, (Seq[EVENT], STATE)],
  ) {
    def events = value.map { case (events, _) => events }
    def and(fn: STATE => Either[String, EVENT]): Sourced[STATE, EVENT] = {
      Sourced(handler, value.flatMap { case (events, state) =>
        fn(state).map { newEvent =>
          val Some(newState) = handler(Some(state), newEvent)
          (events :+ newEvent, newState)
        }
      })
    }
  }
  object Sourced {
    def empty[STATE, EVENT](
      handler: (Option[STATE], EVENT) => Some[STATE],
      value: Either[String, EVENT]
    ): Sourced[STATE, EVENT] = {
      Sourced(handler, value.map { event =>
        val Some(state) = handler(None, event)
        (Seq(event), state)
      })
    }
  }

}

object V6 extends V6Sourced with V4Handler with V5Journal with V5Models
