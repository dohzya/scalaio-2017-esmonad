package esmonad

/**
 * In this version, we use wrap the monad into a function of previous state (technically, a Kleisli).
 */
trait V7Sourced { self: V4Handler =>

  case class SourcedCreation[STATE, EVENT](
    run: Either[String, (Seq[EVENT], STATE)]
  ) {

    def events: Either[String, Seq[EVENT]] = run.map(_._1)

    def andThen(other: SourcedUpdate[STATE, EVENT]): SourcedCreation[STATE, EVENT] = {
      SourcedCreation[STATE, EVENT] {
        this.run.flatMap { case (thisEvents, thisState) =>
          other.run(thisState).map { case (otherEvents, otherState) =>
            (thisEvents ++ otherEvents, otherState)
          }
        }
      }
    }
  }

  case class SourcedUpdate[STATE, EVENT](
    run: (STATE) => Either[String, (Seq[EVENT], STATE)]
  ) {

    def events(state: STATE): Either[String, Seq[EVENT]] = run(state).map(_._1)

    def andThen(other: SourcedUpdate[STATE, EVENT]): SourcedUpdate[STATE, EVENT] = {
      SourcedUpdate[STATE, EVENT] { initialState =>
        this.run(initialState).flatMap { case (thisEvents, thisState) =>
          other.run(thisState).map { case (otherEvents, otherState) =>
            (thisEvents ++ otherEvents, otherState)
          }
        }
      }
    }
  }


  object Sourced {

    def events[STATE, EVENT](state: STATE)(sourcedUpdate: SourcedUpdate[STATE, EVENT]): Either[String, Seq[EVENT]] = {
      sourcedUpdate.events(state)
    }

    final class SourceNewPartiallyApplied[STATE] {
      def apply[EVENT](block: Either[String, EVENT])(implicit handler: EventHandler[STATE, EVENT]): SourcedCreation[STATE, EVENT] = {
        SourcedCreation[STATE, EVENT] {
          block.map { event =>
            (Seq(event), handler(None, event).value)
          }
        }
      }
    }

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]

    def source[STATE, EVENT](block: STATE => Either[String, EVENT])(implicit handler: EventHandler[STATE, EVENT]): SourcedUpdate[STATE, EVENT] = {
      SourcedUpdate[STATE, EVENT] { state =>
        block(state).map { event =>
          (Seq(event), handler(Some(state), event).value)
        }
      }
    }
  }

}

object V7 extends V7Sourced with V4Handler with V5Journal with V5Models
