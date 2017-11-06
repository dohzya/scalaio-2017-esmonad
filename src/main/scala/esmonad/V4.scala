package esmonad

/**
 * Simplest possible implementation of a Sourced monad to accumulate events along some state.
 */
trait V4Sourced { self: FinalHandlers =>

  case class Sourced[STATE, EVENT](run: Either[String, (Vector[EVENT], STATE)]) {

    def events: Either[String, Vector[EVENT]] = run.map { case (events, _) => events }

    def map[B](block: STATE => B): Sourced[B, EVENT] =
      Sourced[B, EVENT](run.map { case (events, state) =>
        (events, block(state))
      })

    def flatMap[B](block: STATE => Sourced[B, EVENT]): Sourced[B, EVENT] =
      Sourced(run.flatMap { case (oldEvents, oldState) =>
        block(oldState).run.map { case (newEvents, newState) =>
          (oldEvents ++ newEvents, newState)
        }
      })

  }

  object Sourced {

    def sourceNew[STATE]: SourceNewPartiallyApplied[STATE] = new SourceNewPartiallyApplied[STATE]

    final class SourceNewPartiallyApplied[STATE] {
      def apply[EVENT](block: Either[String, EVENT])(implicit handler: EventHandler[STATE, EVENT]): Sourced[STATE, EVENT] =
        new Sourced(block.map { e =>
          val Some(newState) = handler(None, e)
          Vector(e) -> newState
        })
    }

    def source[STATE, EVENT](
      block: STATE => Either[String, EVENT]
    )(implicit handler: EventHandler[STATE, EVENT]): STATE => Sourced[STATE, EVENT] =
      state => Sourced(block(state).map { e =>
        val Some(newState) = handler(Some(state), e)
        Vector(e) -> newState
      })

    def source[STATE, EVENT](
      state: STATE,
      block: STATE => Either[String, EVENT]
    )(implicit handler: EventHandler[STATE, EVENT]): Sourced[STATE, EVENT] =
      new Sourced(block(state).map { e =>
        val Some(newState) = handler(Some(state), e)
        Vector(e) -> newState
      })

  }

}

object V4 extends FinalModels with V4Sourced
