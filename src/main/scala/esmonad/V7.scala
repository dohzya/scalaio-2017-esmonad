package esmonad

object V7App extends V7 with App

trait V7 extends V6Handler with V6Journal with V6Model {

  case class SourcedCreation[STATE, ERROR, EVENT](
    run: (EventHandler[STATE, EVENT]) => Either[ERROR, (STATE, Seq[EVENT])]
  ) {

    def events(handler: EventHandler[STATE, EVENT]): Either[ERROR, Seq[EVENT]] = run(handler).map(_._2)

    def and(other: SourcedUpdate[STATE, ERROR, EVENT]): SourcedCreation[STATE, ERROR, EVENT] = {
      SourcedCreation[STATE, ERROR, EVENT] {
        (handler) =>
          this.run(handler).flatMap { case (thisState, thisEvents) =>
            other.run(thisState, handler).map { case (otherState, otherEvents) =>
              (otherState, thisEvents ++ otherEvents)
            }
          }
      }
    }
  }

  case class SourcedUpdate[STATE, ERROR, EVENT](
    run: (STATE, EventHandler[STATE, EVENT]) => Either[ERROR, (STATE, Seq[EVENT])]
  ) {

    def events(state: STATE, handler: EventHandler[STATE, EVENT]): Either[ERROR, Seq[EVENT]] = run(state, handler).map(_._2)

    def and(other: SourcedUpdate[STATE, ERROR, EVENT]): SourcedUpdate[STATE, ERROR, EVENT] = {
      SourcedUpdate[STATE, ERROR, EVENT] {
        (initialState, handler) =>
          this.run(initialState, handler).flatMap { case (thisState, thisEvents) =>
            other.run(thisState, handler).map { case (otherState, otherEvents) =>
              (otherState, thisEvents ++ otherEvents)
            }
          }
      }
    }
  }


  object Sourced {
    def events[STATE, ERROR, EVENT](state: STATE, handler: EventHandler[STATE, EVENT])(sourcedUpdate: SourcedUpdate[STATE, ERROR, EVENT]): Either[ERROR, Seq[EVENT]] = {
      sourcedUpdate.events(state, handler)
    }

    def events[STATE, ERROR, EVENT](handler: EventHandler[STATE, EVENT])(sourcedCreation: SourcedCreation[STATE, ERROR, EVENT]): Either[ERROR, Seq[EVENT]] = {
      sourcedCreation.events(handler)
    }

    def sourceNew[STATE, ERROR, EVENT](block: Either[ERROR, EVENT]): SourcedCreation[STATE, ERROR, EVENT] = {
      SourcedCreation[STATE, ERROR, EVENT] {
        handler =>
          block.map { event =>
            (handler(None, event).value, Seq(event))
          }
      }
    }

    def source[STATE, ERROR, EVENT](block: STATE => Either[ERROR, EVENT]): SourcedUpdate[STATE, ERROR, EVENT] = {
      SourcedUpdate[STATE, ERROR, EVENT] {
        (state, handler) =>
          block(state).map { event =>
            (handler(Some(state), event).value, Seq(event))
          }
      }
    }
  }

}
