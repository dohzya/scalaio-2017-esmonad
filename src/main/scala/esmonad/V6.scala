package esmonad

/**
 * Simplest possible implementation of a Sourced monad to accumulate events along some state.
 */
trait V6Sourced { self: FinalHandlers =>

  case class Sourced[STATE, EVENT](run: Either[String, (Vector[EVENT], STATE)]) {

    def events: Either[String, Vector[EVENT]] = run.map { case (events, _) => events }

    def map[B](fn: STATE => B): Sourced[B, EVENT] =
      Sourced[B, EVENT](run.map { case (events, state) =>
        (events, fn(state))
      })

    def flatMap[B](fn: STATE => Sourced[B, EVENT]): Sourced[B, EVENT] =
      Sourced(run.flatMap { case (oldEvents, oldState) =>
        fn(oldState).run.map { case (newEVents, newState) =>
          (oldEvents ++ newEVents, newState)
        }
      })

    def flatMap[B](block: STATE => Either[String, EVENT]): Sourced[B, EVENT] =
      Sourced(run.flatMap { case (oldEvents, oldState) =>
        fn(oldState).run.map { case (newEVents, newState) =>
          (oldEvents ++ newEVents, newState)
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

object V6 extends FinalModels with V6Sourced
