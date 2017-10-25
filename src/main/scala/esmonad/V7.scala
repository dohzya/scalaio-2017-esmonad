package esmonad

/**
 * In this version, we use wrap the monad into a function of previous state (technically, a Kleisli).
 */
object V7 extends V6Handler with V6Journal with V6Model {

  case class SourcedCreation[STATE, ERROR, EVENT](
    run: Either[ERROR, (Seq[EVENT], STATE)]
  ) {

    def events: Either[ERROR, Seq[EVENT]] = run.map(_._1)

    def andThen(other: SourcedUpdate[STATE, ERROR, EVENT]): SourcedCreation[STATE, ERROR, EVENT] = {
      SourcedCreation[STATE, ERROR, EVENT] {
        this.run.flatMap { case (thisEvents, thisState) =>
          other.run(thisState).map { case (otherEvents, otherState) =>
            (thisEvents ++ otherEvents, otherState)
          }
        }
      }
    }
  }

  case class SourcedUpdate[STATE, ERROR, EVENT](
    run: (STATE) => Either[ERROR, (Seq[EVENT], STATE)]
  ) {

    def events(state: STATE): Either[ERROR, Seq[EVENT]] = run(state).map(_._1)

    def andThen(other: SourcedUpdate[STATE, ERROR, EVENT]): SourcedUpdate[STATE, ERROR, EVENT] = {
      SourcedUpdate[STATE, ERROR, EVENT] { initialState =>
        this.run(initialState).flatMap { case (thisEvents, thisState) =>
          other.run(thisState).map { case (otherEvents, otherState) =>
            (thisEvents ++ otherEvents, otherState)
          }
        }
      }
    }
  }


  object Sourced {

    def events[STATE, ERROR, EVENT](state: STATE)(sourcedUpdate: SourcedUpdate[STATE, ERROR, EVENT]): Either[ERROR, Seq[EVENT]] = {
      sourcedUpdate.events(state)
    }

    final class SourceNewPartiallyApplied[STATE] {
      def apply[ERROR, EVENT](block: Either[ERROR, EVENT])(implicit handler: EventHandler[STATE, EVENT]): SourcedCreation[STATE, ERROR, EVENT] = {
        SourcedCreation[STATE, ERROR, EVENT] {
          block.map { event =>
            (Seq(event), handler(None, event).value)
          }
        }
      }
    }

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]

    def source[STATE, ERROR, EVENT](block: STATE => Either[ERROR, EVENT])(implicit handler: EventHandler[STATE, EVENT]): SourcedUpdate[STATE, ERROR, EVENT] = {
      SourcedUpdate[STATE, ERROR, EVENT] { state =>
        block(state).map { event =>
          (Seq(event), handler(Some(state), event).value)
        }
      }
    }
  }

}
