package esmonad

/**
 * In this version, we use wrap the monad into a function of previous state (technically, a Kleisli).
 */
trait V7Sourced { self: V4Handler =>

  case class SourcedCreation[STATE, EVENT](
    run: Either[String, (Seq[EVENT], STATE)]
  ) {

    def events: Either[String, Seq[EVENT]] = run.map(_._1)

    def andThen[A](other: SourcedUpdate[STATE, EVENT, A]): SourcedCreation[A, EVENT] = {
      SourcedCreation[A, EVENT] {
        this.run.flatMap { case (thisEvents, thisState) =>
          other.run(thisState).map { case (otherEvents, otherState) =>
            (thisEvents ++ otherEvents, otherState)
          }
        }
      }
    }
  }

  case class SourcedUpdate[STATE, EVENT, A](
    run: (STATE) => Either[String, (Seq[EVENT], A)]
  ) {

    def events(state: STATE): Either[String, Seq[EVENT]] = run(state).map(_._1)

    def andThen[B](other: SourcedUpdate[A, EVENT, B]): SourcedUpdate[STATE, EVENT, B] = {
      SourcedUpdate[STATE, EVENT, B] { initialState =>
        this.run(initialState).flatMap { case (thisEvents, thisState) =>
          other.run(thisState).map { case (otherEvents, otherState) =>
            (thisEvents ++ otherEvents, otherState)
          }
        }
      }
    }
  }


  object Sourced {

    def events[STATE, EVENT](sourcedCreation: SourcedCreation[STATE, EVENT]): Either[String, Seq[EVENT]] = {
      sourcedCreation.events
    }

    def events[STATE, EVENT](state: STATE)(sourcedUpdate: SourcedUpdate[STATE, EVENT, _]): Either[String, Seq[EVENT]] = {
      sourcedUpdate.events(state)
    }

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]

    final class SourceNewPartiallyApplied[STATE] {
      def apply[EVENT](block: Either[String, EVENT])(implicit handler: EventHandler[STATE, EVENT]): SourcedCreation[STATE, EVENT] = {
        SourcedCreation[STATE, EVENT] {
          block.map { event =>
            Seq(event) -> handler(None, event).value
          }
        }
      }
    }

    def source[STATE, EVENT](block: STATE => Either[String, EVENT])(implicit handler: EventHandler[STATE, EVENT]): SourcedUpdate[STATE, EVENT, STATE] = {
      SourcedUpdate[STATE, EVENT, STATE] { state =>
        block(state).map { event =>
          Seq(event) -> handler(Some(state), event).value
        }
      }
    }
  }

}

object V7 extends V7Sourced with V4Handler with V5Journal with V5Models
